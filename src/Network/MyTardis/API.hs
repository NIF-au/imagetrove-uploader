{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}

module Network.MyTardis.API where

import Control.Monad.Trans (liftIO)
import Data.Either
import Control.Applicative
import Control.Exception.Base (catch, IOException(..), SomeException(..))
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader

import Data.Aeson
import Data.Aeson.Types
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Traversable (traverse)
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Mime
import Network.Wreq

import Data.Char (toLower, isAlphaNum)

import qualified Network.Connection     as NetworkConnection
import qualified Network.HTTP.Conduit   as NetworkHTTPConduit

import Safe

import Network.MyTardis.Instances

import System.Directory
import System.FilePath
import System.Posix.Files

import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Char8  as B8

import qualified Data.Text as T
import Text.Printf (printf)
import qualified Data.Map as M

import Data.Dicom
import Network.ImageTrove.Utils (computeChecksum, getWithE, postWithE, deleteWithE)
import Network.MyTardis.Types
import Network.MyTardis.RestTypes
import Network.ImageTrove.Utils (runShellCommand)

import System.IO

import System.Unix.Directory (removeRecursiveSafely)

import Data.Time.LocalTime (getZonedTime)

import Control.Concurrent
import Control.Concurrent.STM

writeLog :: String -> ReaderT MyTardisConfig IO ()
writeLog msg = do
    now <- liftIO $ show <$> getZonedTime
    liftIO $ putStrLn (now ++ " :: " ++ msg)
    return ()

data MyTardisConfig = MyTardisConfig
    { myTardisHost       :: String -- ^ URL to MyTARDIS instance, e.g. \"http://localhost:8000\"
    , myTardisApiBase    :: String -- ^ API base. Should be \"\/api\/v1\"
    , myTardisUser       :: String -- ^ Username.
    , myTardisPass       :: String -- ^ Password.
    , myTardisWreqOpts   :: Network.Wreq.Options -- ^ Options for Wreq.
    , imageTroveLogFile  :: Maybe Handle
    , mytardisDir        :: String -- ^ Path to MyTARDIS storage directory, e.g. \"/imagetrove\"
    , mytardisDebug      :: Bool   -- ^ If set to "True" then dicom conversion directories are not removed.
    , mytardisTmp        :: FilePath -- ^ Directory for temporary files. Defaults to \"/tmp\".
    , mytardisApiPrefix  :: String -- ^ Prefix for API URLs, e.g. use \"imagetrove_\" to get \"/imagetrove_experiment\" instead of \"/experiment\".
    , mytardisDCMTkDir   :: Maybe FilePath -- ^ DCMTK incoming directory.
    }
    deriving Show

-- | Helper to create 'MyTardisConfig' with default values.
defaultMyTardisOptions :: String -- ^ MyTARDIS host URL.
                       -> String -- ^ Username
                       -> String -- ^ Password
                       -> String -- ^ MyTARDIS storage directory.
                       -> Bool   -- ^ Debug mode.
                       -> FilePath -- ^ Temporary directory.
                       -> String -- ^ API URL prefix.
                       -> Maybe FilePath -- ^ DCMTK incoming directory.
                       -> MyTardisConfig
defaultMyTardisOptions host user pass mytardisDir debug tmp apiPrefix dcmtkDir = MyTardisConfig host "/api/v1" user pass opts Nothing mytardisDir debug tmp apiPrefix dcmtkDir
  where
    opts = if "https" `isPrefixOf` host then optsHTTPS else optsHTTP
    optsHTTP  = defaults & manager .~ Left (defaultManagerSettings { managerResponseTimeout = Just 3000000000 } )
                         & auth .~ (Just $ basicAuth (B8.pack user) (B8.pack pass))
    optsHTTPS = defaults & manager .~ Left (tlsManager { managerResponseTimeout = Just 3000000000 } )
                         & auth .~ (Just $ basicAuth (B8.pack user) (B8.pack pass))

    -- FIXME The first 'True' to TLSSettingsSimple disables certificate checking. Just for testing at the moment!
    -- See 'settingDisableCertificateValidation' in http://hackage.haskell.org/package/connection-0.2.3/docs/Network-Connection.html
    tlsManager = NetworkHTTPConduit.mkManagerSettings (NetworkConnection.TLSSettingsSimple True False False) Nothing

-- | Wrapper around postWithE that uses our settings in a 'ReaderT'.
postWith' :: String -> Value -> ReaderT MyTardisConfig IO (Either String (Response BL.ByteString))
postWith' x v = do
    MyTardisConfig host apiBase user pass opts _ _ _ _ _ _ <- ask
    writeLog $ "postWith': " ++ show (x, v)
    liftIO $ postWithE opts (host ++ apiBase ++ x) v

-- | Wrapper around Wreq's 'putWith' that uses our settings in a 'ReaderT'.
putWith' :: String -> Value -> ReaderT MyTardisConfig IO (Response BL.ByteString)
putWith' x v = do
    MyTardisConfig host apiBase user pass opts _ _ _ _ _ _ <- ask
    writeLog $ "putWith': " ++ show (x, v)
    liftIO $ putWith opts (host ++ apiBase ++ x) v

-- | Create a resource. See 'createExperiment' for an example use.
createResource
    :: String                                           -- ^ Name of resource used to construct the full URL, e.g. \"/experiment/\"
    -> (String -> ReaderT MyTardisConfig IO (Result a)) -- ^ Function to retrieve the newly created resource.
    -> Value                                            -- ^ Metadata, JSON form.
    -> ReaderT MyTardisConfig IO (Result a)             -- ^ The new resource.
createResource resourceURL getFn keyValueMap = do
    writeLog $ "createResource: " ++ show (resourceURL, keyValueMap)

    r <- postWith' resourceURL (toJSON keyValueMap)

    case r of
        Left e   -> return $ Error e
        Right r' -> do let url = B8.unpack <$> r' ^? responseHeader "Location"

                       writeLog $ "createResource: received url: " ++ show url

                       host <- myTardisHost <$> ask

                       case url of Just url' -> getFn $ drop (length host) url'
                                   Nothing   -> return $ Error $ "Failed to fetch newly created resource: " ++ resourceURL

-- | Update a resource.
updateResource :: (Show a, ToJSON a) =>
       a                                                    -- ^ Resource to update, e.g. 'RestUser'.
    -> (a -> String)                                        -- ^ Function to get URI from the resource.
    -> (String -> ReaderT MyTardisConfig IO (Result a))     -- ^ Function to retrieve the updated resource.
    -> ReaderT MyTardisConfig IO (Result a)                 -- ^ The updated resource.
updateResource resource getURI getFn = do
    writeLog $ "updateResource: " ++ show resource

    apiBase <- myTardisApiBase <$> ask

    r <- putWith' (dropIfPrefix apiBase $ getURI resource) (toJSON resource)

    let url = B8.unpack <$> r ^? responseHeader "Location"

    writeLog $ "updateResource: received url: " ++ show url

    let status = r ^. responseStatus . statusCode

    writeLog $ "updateResource: received status: " ++ show status

    if status `elem` [200, 204]
        then getFn $ getURI resource
        else return $ Error $ "Failed to update resource: " ++ show status

data LookupError = NoMatches | TwoOrMoreMatches | OtherError String deriving (Eq, Show)

-- | Retrieve a resource. Returns 'Right' if exactly one match is found, otherwise 'Left' and some error information.
getResourceWithMetadata :: forall a b. (Show a, Show b, Eq b)
    => ReaderT MyTardisConfig IO (Result [a])   -- ^ Function that retrieves the full list of resources, e.g. 'getExperiments'.
    -> (a -> ReaderT MyTardisConfig IO b)       -- ^ Convert a resource to its "identified" equivalent, e.g. 'IdentifiedExperiment'.
    -> b                                        -- ^ The identified value, e.g. 'IdentifiedExperiment'.
    -> ReaderT MyTardisConfig IO (Either LookupError a) -- ^ A uniquely discovered resource, or a lookup error.
getResourceWithMetadata getFn restToIdentified identifiedObject = do
    objects  <- getFn                                    :: ReaderT MyTardisConfig IO (Result [a])
    objects' <- traverse (mapM restToIdentified) objects :: ReaderT MyTardisConfig IO (Result [b])

    let filtered = filter ((==) identifiedObject . snd) <$> liftM2 zip objects objects'

    writeLog $ "getResourceWithMetadata: |objects|: " ++ show (length <$> objects)
    writeLog $ "getResourceWithMetadata: |objects'|: " ++ show (length <$> objects')
    writeLog $ "getResourceWithMetadata: |filtered|: " ++ show (length <$> filtered)

    return $ case filtered of
        Success [singleMatch] -> Right $ fst singleMatch
        Success []            -> Left NoMatches
        Success _             -> Left TwoOrMoreMatches
        Error e               -> Left $ OtherError e

-- | Given an identified experiment, find a matching experiment in MyTARDIS.
getExperimentWithMetadata :: IdentifiedExperiment -> ReaderT MyTardisConfig IO (Either LookupError RestExperiment)
getExperimentWithMetadata = getResourceWithMetadata getExperiments restExperimentToIdentified

-- | Given an identified dataset, find a matching dataset in MyTARDIS.
getDatasetWithMetadata :: IdentifiedDataset -> ReaderT MyTardisConfig IO (Either LookupError RestDataset)
getDatasetWithMetadata = getResourceWithMetadata getDatasets restDatasetToIdentified

-- | Given an identified file, find a matching file in MyTARDIS. We only look for matching files in the
-- dataset referred to by the 'IdentifiedFile'.
getFileWithMetadata :: IdentifiedFile -> ReaderT MyTardisConfig IO (Either LookupError RestDatasetFile)
getFileWithMetadata idf = getResourceWithMetadata (getDatasetFiles $ idfDatasetURL idf) restFileToIdentified idf

-- | Get the files for a particular dataset.
getDatasetFiles :: URI -> ReaderT MyTardisConfig IO (Result [RestDatasetFile])
getDatasetFiles uri = do
    writeLog $ "getDatasetFiles: uri: " ++ show uri
    MyTardisConfig _ apiBase _ _ _ _ _ _ _ _ _ <- ask
    getList $ dropIfPrefix apiBase $ uri ++ "files"

-- | Create an experiment. If an experiment already exists in MyTARDIS that matches our
-- locally identified experiment, we just return the existing experiment. However,
-- metadata of the existing experiment will be updated (this is to reflect changes
-- in project IDs and operator lists).
createExperiment :: IdentifiedExperiment -> ReaderT MyTardisConfig IO (Result RestExperiment)
createExperiment ie@(IdentifiedExperiment description instutitionName title metadata) = do
    writeLog $ "createExperiment : " ++ show ie
    x <- getExperimentWithMetadata ie

    case x of
        Right re        -> do writeLog "createExperiment: Found existing experiment."
                              writeLog "createExperiment: Updating experiment's metadata."

                              -- FIXME We assume that there is just one metadata schema
                              -- (hence the concat at the front).
                              let metadata' = concat $ map M.toList $ map snd metadata
                              updateExperimentMetadata metadata' re

                              return $ Success re
        Left NoMatches  -> do writeLog "createExperiment: no matches, creating the experiment resource."
                              prefix <- mytardisApiPrefix <$> ask
                              createResource ("/" ++ prefix ++ "experiment/") getExperiment m
        Left  err       -> do writeLog $ "Error when trying to create group: " ++ show err
                              return $ Error $ show err
  where
    m = object
            [ ("description",       String $ T.pack description)
            , ("institution_name",  String $ T.pack instutitionName)
            , ("title",             String $ T.pack title)
            , ("parameter_sets",    toJSON $ map mkParameterSet metadata)
            , ("objectacls",        toJSON $ ([] :: [RestGroup]))
            ]

-- | Worker for creating experiments. Takes identified experiments on the first
-- component of the MVar's tuple and sends results back on the second component of the tuple.
workerCreateExperiment :: MVar (IdentifiedExperiment, MVar (Result RestExperiment)) -> ReaderT MyTardisConfig IO ()
workerCreateExperiment m = forever $ do
    (ie, o) <- liftIO $ takeMVar m

    e <- createExperiment ie

    liftIO $ putMVar o e

callWorker :: MVar (t, MVar b) -> t -> ReaderT MyTardisConfig IO b
callWorker m x = liftIO $ do
    o <- newEmptyMVar
    putMVar m (x, o)
    o' <- takeMVar o
    return o'

-- | Worker for creating datasets. Takes identified datasets on the first
-- component of the MVar's tuple and sends results back on the second component of the tuple.
workerCreateDataset :: MVar (IdentifiedDataset, MVar (Result RestDataset)) -> ReaderT MyTardisConfig IO ()
workerCreateDataset m = forever $ do
    (ids, o) <- liftIO $ takeMVar m
    d <- createDataset ids
    liftIO $ putMVar o d

-- | Worker for creating groups.
workerCreateGroup m = forever $ do
    (name, o) <- liftIO $ takeMVar m
    d <- getOrCreateGroup name
    liftIO $ putMVar o d

-- | Create a new user in MyTARDIS' Django backend.
createUser
    :: Maybe String                                 -- ^ User's first name.
    -> Maybe String                                 -- ^ User's last name.
    -> String                                       -- ^ Username.
    -> [RestGroup]                                  -- ^ Groups that this user is a member of.
    -> Bool                                         -- ^ Create a Django superuser.
    -> ReaderT MyTardisConfig IO (Result RestUser)  -- ^ The new user resource.
createUser firstname lastname username groups isSuperuser = do
    writeLog $ "createUser: " ++ show (firstname, lastname, username, groups, isSuperuser)
    prefix <- mytardisApiPrefix <$> ask
    createResource ("/" ++ prefix ++ "user/") getUser m
  where
    m = object
            [ ("first_name",    String $ T.pack $ fromMaybe "" firstname)
            , ("groups",        toJSON groups)
            , ("last_name",     String $ T.pack $ fromMaybe "" lastname)
            , ("username",      String $ T.pack username)
            , ("is_superuser",  toJSON isSuperuser)
            ]

-- | Construct a ParameterSet from a basic "String"/"String" map.
mkParameterSet :: (String, M.Map String String) -- ^ Schema URL and key/value map.
               -> Value                               -- ^ JSON value.
mkParameterSet (schemaURL, mmap) = object
                                        [ ("schema",     String $ T.pack schemaURL)
                                        , ("parameters", toJSON $ map mkParameter (M.toList mmap))
                                        ]
  where
    mkParameter :: (String, String) -> Value
    mkParameter (name, value) = object
                                    [ ("name",  String $ T.pack name)
                                    , ("value", String $ T.pack value)
                                    ]

mkSharedLocation :: FilePath -> [(FilePath, String)] -> [Value]
mkSharedLocation prefix x = map f x
  where
    f (fname, locname) = object
                            [ ("url",       String $ T.pack $ prefix </> fname)
                            , ("location",  String $ T.pack locname)
                            , ("protocol",  String $ T.pack "file")
                            ]

-- | Create a dataset. If a dataset already exists in MyTARDIS that matches our
-- locally identified dataset, we just return the existing dataset.
createDataset :: IdentifiedDataset -> ReaderT MyTardisConfig IO (Result RestDataset)
createDataset ids@(IdentifiedDataset description experiments metadata) = do
    writeLog $ "createDataset : " ++ show ids

    x <- getDatasetWithMetadata ids

    case x of
        Right rds       -> do writeLog "createDataset: found existing dataset, returning that."
                              return $ Success rds
        Left NoMatches  -> do writeLog "createDataset: no matches found, creating new dataset."
                              prefix <- mytardisApiPrefix <$> ask
                              createResource ("/" ++ prefix ++ "dataset/") getDataset m
        Left  _         -> do writeLog "createDataset: duplicates found, refusing to create another."
                              return $ Error "Duplicate dataset detected, will not create another."

  where
    m = object
            [ ("description",       String $ T.pack description)
            , ("experiments",       toJSON experiments)
            , ("immutable",         toJSON True)
            , ("parameter_sets",    toJSON $ map mkParameterSet metadata)
            ]

-- | Create a Django group. We do not set any permissions
-- here, e.g.  add_logentry, change_logentry, delete_logentry, etc.
createGroup
    :: String                                       -- ^ Group name, e.g. \"Project 12345\".
    -> ReaderT MyTardisConfig IO (Result RestGroup) -- ^ The newly created group.
createGroup name = do
    writeLog $ "createGroup: " ++ show name
    prefix <- mytardisApiPrefix <$> ask
    createResource ("/" ++ prefix ++ "group/") getGroup x
  where
    x = object
            [ ("name",          String $ T.pack name)
            , ("permissions",   toJSON ([] :: [RestPermission]))
            ]

-- | Create an ObjectACL for an experiment.
createExperimentObjectACL
    :: Bool                                             -- ^ canDelete
    -> Bool                                             -- ^ canRead
    -> Bool                                             -- ^ canWrite
    -> RestGroup                                        -- ^ Group for which the ACL refers to.
    -> RestExperiment                                   -- ^ Experiment for which the ACL refers to.
    -> ReaderT MyTardisConfig IO (Result RestObjectACL) -- ^ The new ObjectACL resource.
createExperimentObjectACL canDelete canRead canWrite group experiment = do
    writeLog $ "createExperimentObjectACL : " ++ show (canDelete, canRead, canWrite, group, experiment)
    prefix <- mytardisApiPrefix <$> ask
    createResource ("/" ++ prefix ++ "objectacl/") getObjectACL x
  where
    x = object
            [ ("aclOwnershipType",  toJSON (1 :: Integer))
            , ("content_type",      toJSON ("experiment" :: String))
            , ("canDelete",         toJSON canDelete)
            , ("canRead",           toJSON canRead)
            , ("canWrite",          toJSON canWrite)
            , ("entityId",          String $ T.pack $ show $ groupID group)
            , ("isOwner",           toJSON False)
            , ("object_id",         toJSON $ eiID experiment)
            , ("pluginId",          toJSON ("django_group" :: String))
            ]

-- | For a local file, calculate its canonical file path, md5sum, and size.
calcFileMetadata :: FilePath -> ReaderT MyTardisConfig IO (Maybe (String, String, Integer))
calcFileMetadata _filepath = do
    writeLog $ "calcFileMetadata: _filepath: " ++ _filepath

    filepath <- liftIO $ canonicalizePath _filepath
    writeLog $ "calcFileMetadata: filepath: " ++ filepath

    md5sum   <- liftIO $ computeChecksum filepath
    writeLog $ "calcFileMetadata: md5sum: " ++ show md5sum

    size     <- liftIO $ toInteger . fileSize <$> getFileStatus filepath
    writeLog $ "calcFileMetadata: size: " ++ show size

    return $ case md5sum of
        Right md5sum' -> Just (filepath, md5sum', size)
        Left _        -> Nothing

data FoundDatasetFile = ExistingFile RestDatasetFile | NewFile RestDatasetFile deriving (Eq, Show)

-- | Create a location for a dataset file in MyTARDIS. Fails if a matching file exists in this dataset.
-- It is our responsibility to copy the supplied location. See "copyFileToStore". Until we do so (and the checksums match)
-- the related "RestReplica" will have @replicaVerified == False@.
createFileLocation
    :: Integer                                            -- ^ ID of parent dataset.
    -> IdentifiedFile                                     -- ^ Identified file.
    -> ReaderT MyTardisConfig IO (Result FoundDatasetFile) -- New dataset file resource.
createFileLocation datasetID idf@(IdentifiedFile datasetURL filepath md5sum size metadata) = do

    -- Match on just the base bit of the file, not the whole path.
    let filepath' = takeFileName filepath

    writeLog $ "createFileLocation: idf: " ++ show idf
    writeLog $ "createFileLocation: filepath': " ++ filepath'

    x <- getFileWithMetadata $ idf {idfFilePath = filepath'}

    case x of
        Right dsf -> do writeLog $ "createFileLocation: location already exists, not creating another: " ++ show dsf
                        return $ Success $ ExistingFile dsf -- Error "Matching file exists. We will not create another."
        Left NoMatches        -> let m = object
                                        [ ("dataset",           String $ T.pack datasetURL)
                                        , ("filename",          String $ T.pack $ takeFileName filepath)
                                        , ("md5sum",            String $ T.pack md5sum)
                                        , ("size",              toJSON $ size)
                                        , ("parameter_sets",    toJSON $ map mkParameterSet metadata)
                                        , ("replicas",          toJSON $ mkSharedLocation (show datasetID) [(takeFileName filepath, "imagetrove")]) -- FIXME hardcoded Location name
                                        ] in do writeLog "No matches, creating resource."
                                                prefix <- mytardisApiPrefix <$> ask
                                                dsfNew <- createResource ("/" ++ prefix ++ "dataset_file/") getDatasetFile m
                                                case dsfNew of
                                                    Success dsfNew' -> return $ Success $ NewFile dsfNew'
                                                    Error    e      -> return $ Error e

        Left TwoOrMoreMatches -> do writeLog $ "createFileLocation: Found two or more matches for this file, will not create another: " ++ filepath
                                    return $ Error $ "Found two or more matches for this file, will not create another: " ++ filepath

        Left (OtherError e)   -> do writeLog $ "createFileLocation: Some other error: " ++ show e
                                    return $ Error e

type URI = String

-- | Generic function for creating a resource.
getResource :: forall a. FromJSON a => URI -> ReaderT MyTardisConfig IO (Result a)
getResource uri = do
    writeLog $ "getResource: " ++ uri

    MyTardisConfig host _ _ _ opts _ _ _ _ _ _ <- ask
    r <- liftIO $ getWithE opts (host ++ uri)

    case r of
        Left e -> return $ Error e
        Right r' -> case (join $ decode <$> r' ^? responseBody :: Maybe Value) of
                        Just v  -> return $ fromJSON v
                        Nothing -> do writeLog "getResource: could not decode resource."
                                      return $ Error "Could not decode resource."

-- getParameterName :: URI -> ReaderT MyTardisConfig IO (Result RestParameterName)
-- getParameterName = getResource

getExperimentParameterSet :: URI -> ReaderT MyTardisConfig IO (Result RestExperimentParameterSet)
getExperimentParameterSet = getResource

getExperimentParameter :: URI -> ReaderT MyTardisConfig IO (Result RestParameter)
getExperimentParameter = getResource

getExperiment :: URI -> ReaderT MyTardisConfig IO (Result RestExperiment)
getExperiment = getResource

getSchema :: URI -> ReaderT MyTardisConfig IO (Result RestSchema)
getSchema = getResource

getPermission :: URI -> ReaderT MyTardisConfig IO (Result RestPermission)
getPermission = getResource

getGroup :: URI -> ReaderT MyTardisConfig IO (Result RestGroup)
getGroup = getResource

getObjectACL :: URI -> ReaderT MyTardisConfig IO (Result RestObjectACL)
getObjectACL = getResource

getUser :: URI -> ReaderT MyTardisConfig IO (Result RestUser)
getUser = getResource

getParameter :: URI -> ReaderT MyTardisConfig IO (Result RestParameter)
getParameter = getResource

data MyTardisSchema = SchemaExperiment | SchemaDataset | SchemaDatasetFile | SchemaNone

-- | Create an experiment, dataset, or file schema. The @namespace@ is the primary key in Django, not the @name@.
createSchema
    :: String                                           -- ^ Name, e.g. @DICOM Metadata Experiment@.
    -> String                                           -- ^ Namespace. Must be a url form, e.g. @http://cai.edu.au/schema/1@.
    -> MyTardisSchema                                   -- ^ Schema type.
    -> ReaderT MyTardisConfig IO (Result RestSchema)    -- ^ New schema resource.
createSchema name namespace stype = do
    writeLog $ "createSchema: " ++ show (name, namespace, showSchema stype)
    createResource "/schema/" getSchema m
  where
    m = object
            [ ("name",              String $ T.pack name)
            , ("namespace",         String $ T.pack namespace)
            , ("type",              Number $ fromIntegral $ showSchema stype)
            ]

    showSchema ::  MyTardisSchema -> Integer
    showSchema SchemaExperiment     = 1
    showSchema SchemaDataset        = 2
    showSchema SchemaDatasetFile    = 3
    showSchema SchemaNone           = 4

-- | Retrieve a dataset.
getDataset :: URI -> ReaderT MyTardisConfig IO (Result RestDataset)
getDataset = getResource

-- | Retrieve a dataset file.
getDatasetFile :: URI -> ReaderT MyTardisConfig IO (Result RestDatasetFile)
getDatasetFile = getResource

getTopLevel :: T.Text -> BL.ByteString -> Maybe Value
getTopLevel name b = join $ parseObjects <$> join (decode b)
  where
    parseObjects :: FromJSON a => Object -> Maybe a
    parseObjects = parseMaybe (.: name)

getObjects :: BL.ByteString -> Maybe Value
getObjects = getTopLevel "objects"

getMeta :: BL.ByteString -> Maybe Value
getMeta = getTopLevel "meta"

getList :: forall a. FromJSON a => String -> ReaderT MyTardisConfig IO (Result [a])
getList url = do
    writeLog $ "getList: " ++ url

    MyTardisConfig host apiBase _ _ opts _ _ _ _ _ _ <- ask
    body <- liftIO $ (fmap (^? responseBody)) <$> getWithE opts (host ++ apiBase ++ url)

    case body of
        Left e -> return $ Error e
        Right body' -> do let objects = join $ getObjects <$> body'

                              next   = case fmap emNext <$> (fromJSON <$> join (getMeta <$> body')) of
                                          (Just (Success (Just nextURL))) -> Just $ dropIfPrefix apiBase nextURL
                                          _                               -> Nothing

                          let this = maybeToResult $ fromJSON <$> objects :: Result [a]

                          case next of Just next' -> do rest <- getList next'
                                                        return $ liftM2 (++) this rest
                                       Nothing    -> return this

eitherToResult :: Either String (Result t) -> Result t
eitherToResult (Left e)  = Error e
eitherToResult (Right x) = x

-- This is almost asum from Data.Foldable?
maybeToResult :: Maybe (Result t) -> Result t
maybeToResult (Just x) = x
maybeToResult Nothing  = Error "Nothing"

resultMaybeToResult :: Result (Maybe t) -> Result t
resultMaybeToResult (Success (Just x)) = Success x
resultMaybeToResult (Success Nothing) = Error "Nothing"
resultMaybeToResult (Error e) = Error e

dropIfPrefix :: String -> String -> String
dropIfPrefix prefix s = if prefix `isPrefixOf` s
                            then drop (length prefix) s
                            else s

-- | Retrieve all experiment parameter sets.
getExperimentParameterSets :: ReaderT MyTardisConfig IO (Result [RestExperimentParameterSet])
getExperimentParameterSets = do
    prefix <- mytardisApiPrefix <$> ask
    getList ("/" ++ prefix ++ "experimentparameterset")

-- | Retrieve all experiment parameters.
getExperimentParameters :: ReaderT MyTardisConfig IO (Result [RestParameter])
getExperimentParameters = do
    prefix <- mytardisApiPrefix <$> ask
    getList ("/" ++ prefix ++ "experimentparameter")

-- | Retrieve all experiments.
getExperiments :: ReaderT MyTardisConfig IO (Result [RestExperiment])
getExperiments = do
    prefix <- mytardisApiPrefix <$> ask
    getList ("/" ++ prefix ++ "experiment")

-- | Retrieve all datasets.
getDatasets :: ReaderT MyTardisConfig IO (Result [RestDataset])
getDatasets = do
    prefix <- mytardisApiPrefix <$> ask
    getList ("/" ++ prefix ++ "dataset")

-- | Retrieve all files.
getAllDatasetFiles :: ReaderT MyTardisConfig IO (Result [RestDatasetFile])
getAllDatasetFiles = do
    prefix <- mytardisApiPrefix <$> ask
    getList ("/" ++ prefix ++ "dataset_file")

-- | Retrieve all replicas.
getReplicas :: ReaderT MyTardisConfig IO (Result [RestReplica])
getReplicas = do
    prefix <- mytardisApiPrefix <$> ask
    getList ("/" ++ prefix ++ "replica")

-- | Retrieve all schemas.
getSchemas :: ReaderT MyTardisConfig IO (Result [RestSchema])
getSchemas = getList "/schema"

-- | Retrieve all permissions.
getPermissions :: ReaderT MyTardisConfig IO (Result [RestPermission])
getPermissions = do
    prefix <- mytardisApiPrefix <$> ask
    getList ("/" ++ prefix ++ "permission")

-- | Retrieve all parameter names.
-- getParameterNames :: ReaderT MyTardisConfig IO (Result [RestParameterName])
-- getParameterNames = getList "/parametername"

-- | Retrieve all groups.
getGroups :: ReaderT MyTardisConfig IO (Result [RestGroup])
getGroups = do
    prefix <- mytardisApiPrefix <$> ask
    getList ("/" ++ prefix ++ "group")

-- | Retrieve all users.
getUsers :: ReaderT MyTardisConfig IO (Result [RestUser])
getUsers = getList "/user"

-- | Retrieve all ObjectACLs.
getObjectACLs :: ReaderT MyTardisConfig IO (Result [RestObjectACL])
getObjectACLs = do
    prefix <- mytardisApiPrefix <$> ask
    getList ("/" ++ prefix ++ "objectacl")

-- | Retrieve all replicas that are not verified.
getUnverifiedReplicas :: ReaderT MyTardisConfig IO (Result [RestReplica])
getUnverifiedReplicas = fmap (filter $ not . replicaVerified) <$> getReplicas

-- | Copy a file to the MyTARDIS store. Currently this only supports local files - nothing about web locations
-- (as in the Atom Provider) or an object store. The return value is a list of @Result FilePath@ because a dataset file
-- in MyTARDIS has a list of replicas. Typically we only use one, but for completeness we have to check all of them.
copyFileToStore
    :: FilePath                 -- ^ Path of a local file.
    -> RestDatasetFile          -- ^ Dataset file (should be unverified) with a valid location.
    -> ReaderT MyTardisConfig IO [Result FilePath]   -- ^ List of results of attempting to copy @filepath@ to the location in the dataset file resource.
copyFileToStore filepath dsf = do
    writeLog $ "copyFileToStore: " ++ show (filepath, dsf)

    (mDir :: FilePath) <- mytardisDir <$> ask

    results <- forM (dsfReplicas dsf) $ \r -> do
        let filePrefix = "file://" :: String
            target = replicaURL r
        writeLog $ "copyFileToStore: replica: " ++ show r

        if (not $ replicaVerified r)
            then do let targetFilePath = mDir </> target
                        targetDir      = dropFileName targetFilePath
                    writeLog $ "copyFileToStore: found unverified target: " ++ targetFilePath
                    liftIO $ catch (do createDirectoryIfMissing True targetDir

                                       -- Paranoia - make sure we don't overwrite something.
                                       exists <- doesFileExist targetFilePath

                                       if exists
                                            then return $ Error $ "File already exists: " ++ targetFilePath
                                            else do copyFile filepath targetFilePath
                                                    return (Success targetFilePath))

                                   (\e -> return $ Error $ show (e :: IOException))
            else do writeLog $ "copyFileToStore: replica already verified."
                    return $ Success filepath

    return results

-- | Delete an experiment.
deleteExperiment :: RestExperiment -> ReaderT MyTardisConfig IO (Either String (Response BL.ByteString))
deleteExperiment x = do
    writeLog $ "deleteExperiment: deleting experiment with ID " ++ show (eiID x)
    MyTardisConfig host apiBase _ _ opts _ _ _ _ _ _ <- ask
    liftIO $ deleteWithE opts $ host ++ eiResourceURI x

-- | Delete a dataset.
deleteDataset :: RestDataset -> ReaderT MyTardisConfig IO (Either String (Response BL.ByteString))
deleteDataset x = do
    writeLog $ "deleteDataset: deleting dataset with ID " ++ show (dsiID x)
    MyTardisConfig host apiBase _ _ opts _ _ _ _ _ _ <- ask
    liftIO $ deleteWithE opts $ host ++ dsiResourceURI x

-- | Delete a dataset file.
deleteDatasetFile :: RestDatasetFile -> ReaderT MyTardisConfig IO (Either String (Response BL.ByteString))
deleteDatasetFile x = do
    writeLog $ "deleteDatasetFile: deleting dataset file with ID " ++ show (dsfID x)
    MyTardisConfig host apiBase _ _ opts _ _ _ _ _ _ <- ask
    liftIO $ deleteWithE opts $ host ++ dsfResourceURI x

-- | Delete a parameter.
deleteParameter :: RestParameter -> ReaderT MyTardisConfig IO (Either String (Response BL.ByteString))
deleteParameter x = do
    writeLog $ "deleteParameter: deleting parameter with ID " ++ show (epID x)
    MyTardisConfig host apiBase _ _ opts _ _ _ _ _ _ <- ask
    liftIO $ deleteWithE opts $ host ++ epResourceURI x

restExperimentToIdentified :: RestExperiment -> ReaderT MyTardisConfig IO IdentifiedExperiment
restExperimentToIdentified rexpr = do
    metadata <- mapM handyParameterSet (eiParameterSets rexpr)

    return $ IdentifiedExperiment
                (eiDescription rexpr)
                (eiInstitutionName rexpr)
                (eiTitle rexpr)
                metadata

restDatasetToIdentified :: RestDataset -> ReaderT MyTardisConfig IO IdentifiedDataset
restDatasetToIdentified rds = do
    metadata <- mapM handyParameterSet (dsiParameterSets rds)

    return $ IdentifiedDataset
                (dsiDescription rds)
                (dsiExperiments rds)
                metadata

restFileToIdentified :: RestDatasetFile -> ReaderT MyTardisConfig IO IdentifiedFile
restFileToIdentified rf = do
    metadata <- mapM handyParameterSet (dsfParameterSets rf)

    return $ IdentifiedFile
                (dsfDatasetURL  rf)
                (dsfFilename    rf)
                (dsfMd5sum      rf)
                (read $ dsfSize rf)
                metadata

class GeneralParameterSet a where
    generalGetParameters :: a -> [RestParameter]
    generalGetSchema     :: a -> RestSchema

instance GeneralParameterSet RestExperimentParameterSet where
    generalGetParameters = epsParameters
    generalGetSchema     = epsSchema

instance GeneralParameterSet RestDatasetParameterSet where
    generalGetParameters = dsParamSetParameters
    generalGetSchema     = dsParamSetSchema

instance GeneralParameterSet RestDatasetFileParameterSet where
    generalGetParameters = dsfParamSetParameters
    generalGetSchema     = dsfParamSetSchema

-- | Get a parameterset in a handy (schema, dict) form.
handyParameterSet :: GeneralParameterSet a
    => a  -- An instance of "GeneralParameterSet".
    -> ReaderT MyTardisConfig IO (String, M.Map String String) -- Schema namespace and key/value map.
handyParameterSet paramset = do
    m <- (M.fromList . catMaybes) <$> mapM getBoth (generalGetParameters paramset)
    return (schema, m)

  where

    schema = schemaNamespace $ generalGetSchema paramset

getBoth :: RestParameter -> ReaderT MyTardisConfig IO (Maybe (String, String))
getBoth eparam = do
    name <- getParName eparam
    let value = epStringValue eparam

    case (name, value) of (Success name', Just value') -> return $ Just (name', value')
                          _                            -> return Nothing

getParName :: RestParameter -> ReaderT MyTardisConfig IO (Result String)
-- getParName eparam = fmap pnName <$> getParameterName (epName eparam)
getParName eparam = return $ case epNameCache eparam of
                               ""  -> Error "No name_cache value found"
                               n   -> Success n

-- Get a user if they already exist (matching on the @username@) otherwise create the user.
-- If a user already exists and they have @isSuperuser == False@ and we call this function
-- with @isSuperuser == True@, we will not change them to a superuser. Similarly, calling
-- this function with changing values for @groups@ will not update the user's group membership.
getOrCreateUser
    :: Maybe String     -- ^ User's first name.
    -> Maybe String     -- ^ User's last name.
    -> String           -- ^ Username.
    -> [RestGroup]      -- ^ Rest groups that the user should be a member of.
    -> Bool             -- ^ Create as a superuser.
    -> ReaderT MyTardisConfig IO (Result RestUser)
getOrCreateUser firstname lastname username groups isSuperuser = do
    writeLog $ "getOrCreateUser: " ++ show (firstname, lastname, username, groups, isSuperuser)
    users <- traverse (filter ((username ==) . ruserUsername)) <$> getUsers

    case users of
        []              -> do writeLog $ "getOrCreateUser: no user found, creating new user resource."
                              createUser firstname lastname username groups isSuperuser 
        [Success user]  -> do writeLog $ "getOrCreateUser: found matching user: " ++ show username
                              return $ Success user
        duplicates      -> do writeLog $ "getOrCreateUser: duplicate user accounts! " ++ show (username, duplicates)
                              return $ Error $ "Duplicate users found with username: " ++ username

-- | Get a group with a given name, or create it if it doesn't already exist. Will fail if
-- a duplicate group name is discovered.
getOrCreateGroup  :: String -> ReaderT MyTardisConfig IO (Result RestGroup)
getOrCreateGroup name = do
    writeLog $ "getOrCreateGroup: " ++ name
    groups <- traverse (filter $ (==) name . groupName) <$> getGroups

    case groups of
        []              -> createGroup name
        [Success group] -> return $ Success group
        groups          -> return $ Error $ "Error when creating group " ++ name ++ ": " ++ show groups

-- | Get or create an ObjectACL that gives a group specified access to an experiment.
getOrCreateExperimentACL
    :: Bool                 -- ^ canDelete
    -> Bool                 -- ^ canRead
    -> Bool                 -- ^ canWrite
    -> RestExperiment       -- ^ Experiment.
    -> RestGroup            -- ^ Group.
    -> ReaderT MyTardisConfig IO (Result RestObjectACL) -- ^ Updated ObjectACL.
getOrCreateExperimentACL canDelete canRead canWrite experiment group = do
    writeLog $ "getOrCreateExperimentACL: " ++ show (canDelete, canRead, canWrite, experiment, group)
    acls <- traverse (filter f) <$> getObjectACLs

    case acls of
        []            -> do writeLog $ "getOrCreateExperimentACL: no ACLs found, creating new one."
                            createExperimentObjectACL canDelete canRead canWrite group experiment
        [Success acl] -> do writeLog $ "getOrCreateExperimentACL: found matching ACL."
                            return $ Success acl
        duplicates    -> do writeLog $ "getOrCreateExperimentACL: duplicate ACLs found: " ++ show duplicates
                            return $ Error $ "Duplicate Object ACLs found for: " ++ show experiment ++ " " ++ show group

  where

    f acl =  objectAclOwnershipType acl == 1
          && objectCanDelete        acl == canDelete
          && objectCanRead          acl == canRead
          && objectCanWrite         acl == canWrite
          && objectEntityId         acl == show (groupID group)
          && objectIsOwner          acl == False
          && objectObjectID         acl == eiID experiment
          && objectPluginId         acl == "django_group"

-- | Make a user a member of a group.
addUserToGroup  :: RestUser -> RestGroup -> ReaderT MyTardisConfig IO (Result RestUser)
addUserToGroup user group = do
    writeLog $ "addUserToGroup: " ++ show (user, group)

    if groupName group `elem` map groupName currentGroups
        then return $ Success user
        else updateResource (user { ruserGroups = group:currentGroups }) ruserResourceURI getUser
  where
    currentGroups = ruserGroups user

-- | Remove a group from a user's membership list.
removeUserFromGroup  :: RestUser -> RestGroup -> ReaderT MyTardisConfig IO (Result RestUser)
removeUserFromGroup user group = do
    writeLog $ "removeUserFromGroup: " ++ show (user, group)
    updateResource user' ruserResourceURI getUser
  where
    user' = user { ruserGroups = filter (group /=) (ruserGroups user) }

-- | Allow a group to have read-only access to an experiment.
addGroupReadOnlyAccess  :: RestExperiment -> RestGroup -> ReaderT MyTardisConfig IO (Result RestExperiment)
addGroupReadOnlyAccess = addGroupAccess False True False

-- | Add the specified group access to an experiment.
addGroupAccess
    :: Bool                                                 -- ^ canDelete
    -> Bool                                                 -- ^ canRead
    -> Bool                                                 -- ^ canWrite
    -> RestExperiment                                       -- ^ Experiment.
    -> RestGroup                                            -- ^ Group.
    -> ReaderT MyTardisConfig IO (Result RestExperiment)    -- ^ Updated experment resource.
addGroupAccess canDelete canRead canWrite experiment group = do
    writeLog $ "addGroupAccess: " ++ show (canDelete, canRead, canWrite, experiment, group)
    acl <- getOrCreateExperimentACL canDelete canRead canWrite experiment group

    -- Note: we don't need to update the experiment, we just have
    -- to create the ObjectACL referring to the Experiment's ID.

    case acl of
        Success acl' -> getExperiment (eiResourceURI experiment)
        Error   e    -> do writeLog $ "addGroupAccess: error: " ++ e
                           return $ Error e

uploadFileBasic
    :: String
    -> (RestDataset -> String -> String -> Integer -> [(String, M.Map String String)] -> IdentifiedFile)
    -> RestDataset
    -> FilePath
    -> [(String, M.Map String String)]
    -> ReaderT MyTardisConfig IO (Result RestDatasetFile)
uploadFileBasic schemaFile identifyDatasetFile d f m = do
    writeLog $ "uploadFileBasic: " ++ show (schemaFile, d, f, m)
    meta <- calcFileMetadata f

    -- This would be tidier if we worked in the Success monad?
    case meta of
        Just  (filepath, md5sum, size) -> do let idf = identifyDatasetFile d filepath md5sum size m
                                             dsf <- createFileLocation (dsiID d) idf

                                             case dsf of (Success (NewFile dsf')) -> do results <- copyFileToStore f dsf'
                                                                                        case allSuccesses results of
                                                                                            Success _ -> do writeLog $ "uploadFileBasic: created file location: " ++ show dsf'
                                                                                                            return $ Success dsf'
                                                                                            Error e   -> do writeLog $ "uploadFileBasic: error while copying file: " ++ e
                                                                                                            return $ Error e
                                                         (Success (ExistingFile dsf')) -> do writeLog $ "uploadFileBasic: warning, ignoring existing dataset file: " ++ show dsf'
                                                                                             return $ Success dsf' -- FIXME log a message here
                                                         Error e      -> do writeLog $ "uploadFileBasic: error creating file location resource: " ++ e
                                                                            return $ Error e
        Nothing                        -> do writeLog $ "uploadFileBasic: Failed to calculate checksum for " ++ f
                                             return $ Error $ "Failed to calculate checksum for " ++ f

allSuccesses :: [Result a] -> Result String
allSuccesses []              = Success "All Success or empty list."
allSuccesses (Success _:xs) = allSuccesses xs
allSuccesses (Error e:_)    = Error e

allRights :: [Either a b] -> Either a String
allRights []           = Right "All Right or empty list."
allRights (Right _:xs) = allRights xs
allRights (Left e:_)   = Left e

squashResults :: Result (Result t) -> Result t
squashResults (Success (Success x)) = Success x
squashResults (Success (Error e))   = Error $ "Error in inner Result: " ++ e
squashResults (Error e) = Error e

createSchemasIfMissing :: (String, String, String) -> ReaderT MyTardisConfig IO (Result (RestSchema, RestSchema, RestSchema))
createSchemasIfMissing (schemaExperiment, schemaDataset, schemaFile) = do
    writeLog $ "createSchemasIfMissing: " ++ show (schemaExperiment, schemaDataset, schemaFile)
    schemas <- getSchemas

    case schemas of
        Error e -> return $ Error e
        Success schemas' -> do experimentSchema <- createIfMissing "Experiment Metadata" schemaExperiment SchemaExperiment  schemas'
                               datasetSchema    <- createIfMissing "Dataset Metadata"    schemaDataset    SchemaDataset     schemas'
                               fileSchema       <- createIfMissing "File Metadata"       schemaFile       SchemaDatasetFile schemas'

                               case (experimentSchema, datasetSchema, fileSchema) of
                                         (Success experimentSchema', Success datasetSchema', Success fileSchema') -> return $ Success (experimentSchema', datasetSchema', fileSchema')
                                         (Error e, _, _)  -> do writeLog $ "createSchemasIfMissing: failed to create schema: " ++ e
                                                                return $ Error $ "Failed to create schema (which one?)." ++ e
                                         (_, Error e, _)  -> do writeLog $ "createSchemasIfMissing: failed to create schema: " ++ e
                                                                return $ Error $ "Failed to create schema (which one?)." ++ e
                                         (_, _, Error e)  -> do writeLog $ "createSchemasIfMissing: failed to create schema: " ++ e
                                                                return $ Error $ "Failed to create schema (which one?)." ++ e

  where

    schemaExists  :: String -> [RestSchema] -> Bool
    schemaExists ns schemas = any ((==) ns . schemaNamespace) schemas

    -- createIfMissing :: String -> [RestSchema] -> ???
    createIfMissing name namespace stype schemas = if schemaExists namespace schemas
                                    then return $ Success $ head $ filter ((==) namespace . schemaNamespace) schemas
                                    else createSchema name namespace stype

-- | Convert the DICOM files in a directory to MINC and upload.
uploadDicomAsMinc
  :: [DicomFile -> Bool]                    -- ^ Filters to match instrument.
     -> [DicomFile -> Maybe String]         -- ^ Fields to use in the instrument metadata field.
     -> [DicomFile -> Maybe String]         -- ^ Fields to use in the experiment's title.
     -> [DicomFile -> Maybe String]         -- ^ Fields to use in the dataset's title.
     -> (String
         -> String
         -> String
         -> String
         -> [String]
         -> [DicomFile -> Maybe String]
         -> [DicomFile -> Maybe String]
         -> [DicomFile]
         -> Either String IdentifiedExperiment)     -- ^ Identify an experiment.
     -> (String
         -> [DicomFile -> Maybe String]
         -> RestExperiment
         -> [DicomFile]
         -> Maybe IdentifiedDataset)        -- ^ Identify a dataset.
     -> (RestDataset
         -> String
         -> String
         -> Integer
         -> [(String, M.Map String String)]
         -> IdentifiedFile)                 -- ^ Identify a file.
     -> FilePath                            -- ^ Directory containing DICOM files.
     -> (String, String, String, String, String, String, [String]) -- ^ Experiment schema, dataset schema, file schema, default institution name, default department name, default institution address, default operators.
     -> ReaderT MyTardisConfig IO ()

uploadDicomAsMinc instrumentFilters instrumentMetadataFields experimentFields datasetFields identifyExperiment identifyDataset identifyDatasetFile dir (schemaExperiment, schemaDataset, schemaFile, defaultInstitutionName, defaultInstitutionalDepartmentName, defaultInstitutionalAddress, defaultOperators) = do
    _files1 <- liftIO $ rights <$> (getDicomFilesInDirectory ".dcm" dir >>= mapM readDicomMetadata)
    _files2 <- liftIO $ rights <$> (getDicomFilesInDirectory ".IMA" dir >>= mapM readDicomMetadata)
    let _files = _files1 ++ _files2
    liftIO $ putStrLn $ "uploadDicomAsMinc: |_files| = " ++ (show $ length _files)

    let groups = groupDicomFiles instrumentFilters experimentFields datasetFields _files
    liftIO $ putStrLn $ "uploadDicomAsMinc: |groups| = " ++ (show $ length groups)

    forM_ groups $ \files -> do result <- uploadDicomAsMincOneGroup undefined undefined files instrumentFilters instrumentMetadataFields experimentFields datasetFields identifyExperiment identifyDataset identifyDatasetFile dir (schemaExperiment, schemaDataset, schemaFile, defaultInstitutionName, defaultInstitutionalDepartmentName, defaultInstitutionalAddress, defaultOperators)
                                liftIO $ putStrLn $ " uploadDicomAsMinc: result from uploadDicomAsMincOneGroup: " ++ show result

uploadDicomAsMincOneGroup experimentMVar datasetMVar files instrumentFilters instrumentMetadataFields experimentFields datasetFields identifyExperiment identifyDataset identifyDatasetFile dir (schemaExperiment, schemaDataset, schemaFile, defaultInstitutionName, defaultInstitutionalDepartmentName, defaultInstitutionalAddress, defaultOperators) = do
    writeLog $ "uploadDicomAsMincOneGroup: " ++ show (files, dir, (schemaExperiment, schemaDataset, schemaFile, defaultInstitutionName, defaultInstitutionalDepartmentName, defaultInstitutionalAddress, defaultOperators))

    writeLog $ "Getting schemas."
    schemas <- createSchemasIfMissing (schemaExperiment, schemaDataset, schemaFile)
 
    writeLog $ "Identifying experiment."
    let ie = identifyExperiment
                schemaExperiment
                defaultInstitutionName
                defaultInstitutionalDepartmentName
                defaultInstitutionalAddress
                defaultOperators
                experimentFields
                instrumentMetadataFields
                files

    writeLog $ "Creating experiment."
    e <- eitherToResult <$> traverse (callWorker experimentMVar) ie :: ReaderT MyTardisConfig IO (Result RestExperiment)

    writeLog $ "Identifying dataset."
    let ids = resultMaybeToResult ((\e -> identifyDataset schemaDataset datasetFields e files) <$> e) :: Result IdentifiedDataset

    writeLog $ "Creating dataset."
    -- d <- squashResults <$> traverse createDataset ids :: ReaderT MyTardisConfig IO (Result RestDataset)
    d <- squashResults <$> traverse (callWorker datasetMVar) ids :: ReaderT MyTardisConfig IO (Result RestDataset)

    let oneFile = headMay files
        Just pid        = join (dicomPatientID         <$> oneFile) -- FIXME dangerous pattern match, should be an error
        Just studyDesc  = join (dicomStudyDescription  <$> oneFile) -- FIXME dangerous pattern match, should be an error
        Just seriesDesc = join (dicomSeriesDescription <$> oneFile) -- FIXME dangerous pattern match, should be an error
        seriesNr        = fromMaybe "(series)" $join (dicomSeriesNumber <$> oneFile)
        filemetadata = (\f -> [(schemaFile, M.fromList
                                                [ ("PatientID",         fromMaybe "(PatientID missing)"         $ dicomPatientID         f)
                                                -- , ("StudyInstanceUID",  fromMaybe "(StudyInstanceUID missing)"  $ dicomStudyInstanceUID  f)
                                                -- , ("SeriesInstanceUID", fromMaybe "(SeriesInstanceUID missing)" $ dicomSeriesInstanceUID f)
                                                , ("StudyDescription",  fromMaybe "(StudyDescription missing)"  $ dicomStudyDescription  f)
                                                ]
                               )
                              ]) <$> oneFile

    -- Pack the dicom files as Minc
    writeLog $ "Converting DICOM to Minc."
    tmp <- mytardisTmp <$> ask
    dicom <- liftIO $ dicomToMinc tmp $ map dicomFilePath files

    case (e, d, filemetadata) of
        (Success e', Success d', Just filemetadata') -> renameMincThenFinal e d finaliseMincFiles schemaFile identifyDatasetFile d' filemetadata' pid studyDesc seriesDesc seriesNr dicom
        (Error expError, _, _)                       -> do return $ Error $ "Error when creating experiment: " ++ expError
        (Success _, Error dsError, _)                -> do return $ Error $ "Error when creating dataset: " ++ dsError
        (Success _, Success _, Nothing)              -> do return $ Error $ "Error when creating extracting file metadata. No files in DICOM group!"

-- FIXME This has become a mess. If the 'dicom' parameter is a Left, it means that the earlier dicomToMinc failed. Which is entirely
-- reasonable because not all DICOM data can be converted to Minc. So let's return a Success here (ugh) even though something
-- went wrong earlier.
-- renameMincThenFinal e d finaliseMincFiles schemaFile identifyDatasetFile d' filemetadata' pid studyDesc seriesDesc seriesNr (Left err) = return $ Error err
renameMincThenFinal e d finaliseMincFiles schemaFile identifyDatasetFile d' filemetadata' pid studyDesc seriesDesc seriesNr (Left err) = return $ Success (e, d)


renameMincThenFinal e d finaliseMincFiles schemaFile identifyDatasetFile d' filemetadata' pid studyDesc seriesDesc seriesNr dicom@(Right (tempDir, _mincFiles)) = do
    -- Rename the minc files to have study and series prefixes.
    -- FIXME Surprising that 'rename' has type Filepath -> FilePath -> IO ()
    -- How do we know if the rename worked!?
    liftIO $ mapM (\f -> rename f (replacePatientName f)) _mincFiles

    let mincFiles = map replacePatientName _mincFiles

    finaliseMincFiles schemaFile identifyDatasetFile d' filemetadata' (tempDir, mincFiles)

    debug <- mytardisDebug <$> ask

    if debug
        then writeLog $ "Not removing temporary directory: " ++ tempDir
        else do writeLog $ "Removing temporary directory: " ++ tempDir
                liftIO $ removeRecursiveSafely tempDir

    -- FIXME This should depend on the earlier results!
    return $ Success (e, d)

  where
    pid'        = filter isAlphaNum pid          -- FIXME Could be empty?
    studyDesc'  = filter isAlphaNum studyDesc    -- FIXME Could be empty?
    seriesDesc' = filter isAlphaNum seriesDesc   -- FIXME Could be empty?
    seriesNr'   = if seriesNr == "(series)" then seriesNr else filter isAlphaNum seriesNr   -- FIXME Could be empty?

    dropPatientName = drop (1 + length pid)
    addPrefix f     = pid' ++ "_" ++ studyDesc' ++ "_" ++ seriesDesc' ++ "_" ++ seriesNr' ++ "_" ++ f

    replacePatientName f = if (tidyName pid) `isPrefixOf` oldFileName
                                then joinPath $ firstBits ++ [newFileName]
                                else error $ "Unexpected MINC filename: " ++ show (pid, oldFileName, newFileName, f)
       where bits        = splitPath f
             firstBits   = take (length bits - 1) bits -- FIXME fails if path is empty...
             oldFileName = last bits
             newFileName = (addPrefix . dropPatientName) oldFileName

             tidyName = map (toLower . fixWeirdChars)

             fixWeirdChars '-' = '_'
             fixWeirdChars '.' = '_'
             fixWeirdChars ':' = '_'
             fixWeirdChars c   = c

data FinalResult = BothOK
                 | MincFileError  String
                 | ThumbnailError String
                 | NiftiError     String
                 deriving (Eq, Show)

anyCatastrohpicErrors :: [(FinalResult, a, b)] -> Bool
anyCatastrohpicErrors []                            = False
anyCatastrohpicErrors ((MincFileError _, _, _):_)   = True
anyCatastrohpicErrors (_:fs)                        = anyCatastrohpicErrors fs

finaliseMincFiles
    :: String   -- ^ Schema URL for files.
    -> (RestDataset -> String -> String -> Integer -> [(String, M.Map String String)] -> IdentifiedFile) -- ^ Identify a dataset.
    -> RestDataset  -- ^ Dataset that we are adding the file to.
    -> [(String, M.Map String String)] -- ^ File metadata map.
    -> (FilePath, [FilePath])           -- ^ Temporary directory containing MINC files; list of MINC files in that directory.
    -> ReaderT MyTardisConfig IO [(FinalResult, Result RestDatasetFile, Either String (Result RestDatasetFile), Either String (Result RestDatasetFile))]
finaliseMincFiles schemaFile identifyDatasetFile d filemetadata (tempDir, mincFiles) = do

    tmp <- mytardisTmp <$> ask
    toMinc2Results <- liftIO $ forM mincFiles (mncToMnc2 tmp) -- FIXME check results

    stuff <- forM mincFiles $ \f -> do
        writeLog $ "uploadDicomAsMinc: minc file f: " ++ show f
        dsf <- uploadFileBasic schemaFile identifyDatasetFile d f filemetadata
        writeLog $ "uploadDicomAsMinc: dsf: " ++ show dsf

        thumbnail <- liftIO $ createMincThumbnail f
        writeLog $ "uploadDicomAsMinc: thumbnail: " ++ show thumbnail

        nifti <- liftIO $ createNifti f
        writeLog $ "uploadDicomAsMinc: nifti: " ++ show nifti

        dsft <- traverse (\thm -> uploadFileBasic schemaFile identifyDatasetFile d thm filemetadata) thumbnail

        niftiResult <- traverse (\n -> uploadFileBasic schemaFile identifyDatasetFile d n filemetadata) nifti

        case (dsf, dsft, niftiResult) of
            (Error e, _, _)                       -> do writeLog $ "Error while uploading file: " ++ e
                                                        writeLog $ "See also the directory: " ++ tempDir
                                                        return (MincFileError e, dsf, dsft, niftiResult)
            (Success dsf', Right (Success dsft'), Right (Success niftiResult')) -> do writeLog $ "Successfully uploaded file: " ++ f
                                                                                      writeLog $ "Successfully uploaded thumbnail: " ++ show thumbnail
                                                                                      writeLog $ "Successfully uploaded nifti: " ++ show nifti
                                                                                      return (BothOK, dsf, dsft, niftiResult)
            (Success dsf', Right (Error e), _)    -> do writeLog $ "Error when creating thumbnail dataset file: " ++ e
                                                        return (ThumbnailError e, dsf, dsft, niftiResult)
            (Success dsf', Left e, _)             -> do writeLog $ "Error (http?) when creating thumbnail dataset file: " ++ e
                                                        return (ThumbnailError e, dsf, dsft, niftiResult)
            (Success dsf', Right _, Left e)       -> do writeLog $ "Error (http?) when creating nifti file: " ++ e
                                                        return (NiftiError e, dsf, dsft, niftiResult)

    debug <- mytardisDebug <$> ask

    if debug
        then writeLog $ "Not removing temporary directory: " ++ tempDir
        else do writeLog $ "Removing temporary directory: " ++ tempDir
                liftIO $ removeRecursiveSafely tempDir

    return stuff


-- | Update an experiment parameter value.
updateParameter ((name, value), rp) (name', value') = do
    if name == name' && value /= value'
        then do liftIO $ print (name, value, value')
                updateResource (rp { epStringValue = Just value' }) epResourceURI getParameter
                return ()
        else return ()
  
updateExperimentMetadata :: [(String, String)] -> RestExperiment -> ReaderT MyTardisConfig IO ()
updateExperimentMetadata newMetadata e = do
    let parameterSets  = eiParameterSets e
        restParameters = concat $ map epsParameters parameterSets

    nameAndValues <- concat <$> mapM (\z -> (mapM getBoth) $ epsParameters z) parameterSets

    let together = zip nameAndValues restParameters

    let getNones (Nothing, rp) = [rp]
        getNones _             = []

    forM_ (concat $ map getNones together) $ \r -> do
        liftIO $ putStrLn $ "Error: could not look up name for parameter: " ++ show r
        liftIO $ putStrLn $ "Error: metadata for experiment " ++ (show $ eiID e) ++ " may be inaccurate!"

    let getJusts (Just nv, rp) = [(nv, rp)]
        getJusts (Nothing, _)  = []

    forM_ newMetadata $ \newm -> do
        forM_ (concat $ map getJusts together) $ \o -> do
            updateParameter o newm
