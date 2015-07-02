{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module Network.ImageTrove.Acid (getLastRunTime, setLastRunTime) where
module Network.ImageTrove.Acid (acidWorker, callWorkerIO, AcidAction(..), AcidOutput(..), Key(..)) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import Data.Time.LocalTime (ZonedTime(..))
import Data.Typeable

import Control.Concurrent
import Control.Concurrent.STM

import System.Random (randomIO)

import qualified Data.Map as Map

-- import Control.Exception (onException)
import Control.Concurrent (threadDelay)

import Control.Monad.Catch

-- Key/Value example copied from acid-state example: https://github.com/acid-state/acid-state/blob/master/examples/KeyValue.hs

type Key   = String
type Value = ZonedTime

data KeyValue = KeyValue !(Map.Map Key Value) deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value = do
    KeyValue m <- get
    put (KeyValue (Map.insert key value m))

deleteKey :: Key -> Update KeyValue ()
deleteKey key = do
    KeyValue m <- get
    put (KeyValue (Map.delete key m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key = do
    KeyValue m <- ask
    return (Map.lookup key m)

getMapInternal :: Query KeyValue (Map.Map Key Value)
getMapInternal = do
    KeyValue m <- ask
    return m

$(makeAcidic ''KeyValue ['insertKey, 'deleteKey, 'lookupKey, 'getMapInternal])

loadMap :: AcidState (EventState GetMapInternal) -> FilePath -> IO (Map.Map Key Value)
loadMap acid fp = do
    m <- query acid GetMapInternal
    return m

updateLastUpdate :: AcidState (EventState GetMapInternal) -> FilePath -> Key -> ZonedTime -> IO ()
updateLastUpdate acid fp x lastUpdate = do
    _ <- update acid (InsertKey x lastUpdate)
    return ()

deleteLastUpdate :: AcidState (EventState GetMapInternal) -> FilePath -> Key -> IO ()
deleteLastUpdate acid fp x = do
    _ <- update acid (DeleteKey x)
    return ()

data AcidAction = AcidLoadMap FilePath
                | AcidUpdateMap FilePath Key ZonedTime
                deriving Show

data AcidOutput = AcidMap (Map.Map Key Value)
                | AcidNothing
                deriving Show

acidWorker fp m = do
    acid <- openLocalStateFrom fp (KeyValue Map.empty)
    (action acid) `finally` (closeAcidState acid)
  where
    action acid = forever $ do
                (action, o) <- takeMVar m

                case action of
                    AcidLoadMap fp -> do m <- loadMap acid fp
                                         putMVar o (AcidMap m)

                    AcidUpdateMap fp x lastUpdate -> do updateLastUpdate acid fp x lastUpdate
                                                        putMVar o AcidNothing

-- FIXME Copied from API.hs
callWorkerIO :: MVar (t, MVar b) -> t -> IO b
callWorkerIO m x = do
    o <- newEmptyMVar
    putMVar m (x, o)
    o' <- takeMVar o
    return o'

{-
-- Stuff to turn into a command line option
_foo fp = do
    m <- loadMap fp
    let m' = Map.toList m

    -- let someone = filter (\((p,_,_),_) -> p == "4837bc2a-557ab998-ee29fd37-bbe498a8-6b3430cc") m'

    -- forM_ someone print

    -- Also add option to delete a patient.

    -- *Network.ImageTrove.Acid> m <- loadMap "state_sample_config_files_CAI_7T.conf"
    -- *Network.ImageTrove.Acid> let m' = Map.toList m
    -- *Network.ImageTrove.Acid> let x = filter (\((p,_,_),_) -> p == "HASH OF PATIENT") m'
    -- *Network.ImageTrove.Acid> x
    -- *Network.ImageTrove.Acid> forM_ (map (\(z,_) -> z) x) (deleteLastUpdate "state_sample_config_files_CAI_7T.conf")

    print ()
-}

