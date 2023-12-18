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

import InternalPrelude

-- * Helpers

import Data.Time qualified as Time

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
