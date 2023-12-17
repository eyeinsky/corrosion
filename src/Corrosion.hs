module Corrosion
  ( module Corrosion
  , module Export
  ) where

-- * Re-exports

import InternalPrelude as Export (bool, (^))
import Core as Export
import Filesystem as Export
import Builtins as Export
import Coreutils as Export

import Control.Monad as Export (forM, forM_, when)
import Control.Exception as Export (throwIO)

-- * Helpers

import Data.Time qualified as Time
import Options.Applicative qualified as O

-- * CLI

simpleCli :: String -> O.Parser a -> IO a
simpleCli header cli = O.execParser $ O.info (O.helper <*> cli) $ O.fullDesc <> O.header header

headerDescrCli :: String -> String -> O.Parser a -> IO a
headerDescrCli header descr cli = O.execParser $ O.info (O.helper <*> cli) $ O.fullDesc <> O.header header <> O.progDesc descr

-- * Measure time

timeIO :: IO a -> IO (a, Time.NominalDiffTime)
timeIO action = do
  t0 <- Time.getCurrentTime
  a <- action
  t1 <- Time.getCurrentTime
  return (a, Time.diffUTCTime t1 t0)

timePrintPrim :: (Time.NominalDiffTime -> IO ()) -> IO a -> IO a
timePrintPrim print_ action = do
  (a, time) <- timeIO action
  print_ time $> a

timePrint :: String -> IO a -> IO a
timePrint label action = timePrintPrim (\time -> putStrLn $ label <> " took " <> show time) action

timePrint_ :: IO a -> IO a
timePrint_ action = timePrint "_" action

todo :: a
todo = undefined

-- * Generic

labelPrint :: (MonadIO m, Show a) => String -> a -> m ()
labelPrint label a = liftIO $ putStrLn $ label <> " " <> show a

-- * (,) is (:=)

type (:=) a b = (a, b)

pattern (:=) :: a -> b -> a := b
pattern (:=) a b = (a, b)

infixr 0 :=

{-# COMPLETE (:=) #-}
