module Corrosion
  ( module Corrosion
  , module Export
  ) where

-- * Re-exports

import Core as Export
import Builtins as Export
import Coreutils as Export

import Control.Monad as Export (forM, forM_, when)
import Control.Exception as Export (throwIO)

-- * Helpers

import Streaming.Prelude qualified as S
import Data.Time qualified as Time
import Options.Applicative qualified as O
import Data.List qualified as L

bool :: a -> a -> Bool -> a
bool a b t = if t then a else b

boolEither :: a -> b -> Bool -> Either a b
boolEither l r t = bool (Left l) (Right r) t

infixr 9 ^
(^) :: (a -> b) -> (b -> c) -> a -> c
(^) = flip (.)

-- * CLI

simpleCli :: String -> O.Parser a -> IO a
simpleCli header cli = O.execParser $ O.info (O.helper <*> cli) $ O.fullDesc <> O.header header

headerDescrCli :: String -> String -> O.Parser a -> IO a
headerDescrCli header descr cli = O.execParser $ O.info (O.helper <*> cli) $ O.fullDesc <> O.header header <> O.progDesc descr

-- * File system

type DirOrFilePath = Either FilePath FilePath
--                          ^ dir    ^ file

-- ** List

-- | List directory content as relative paths
lsPrim :: FilePath -> IO [DirOrFilePath]
lsPrim root = do
  paths :: [FilePath] <- listDirectory root
  forM paths $ \name -> boolEither name name <$> doesDirectoryExist (root <> "/" <> name)

-- | List directory content, shallow
ls :: FilePath -> IO [DirOrFilePath]
ls root = map (bimap addRoot addRoot) <$> lsPrim root
  where addRoot name = root <> "/" <> name

-- | List directory content, recursive
lsRecursive :: FilePath -> IO [DirOrFilePath]
lsRecursive root = ls root >>= fmap concat . mapM \case
  Left dir -> (Left dir :) <$> lsRecursive dir
  Right file -> pure $ [Right file]

-- ** Streaming

ls' :: FilePath -> Shell_ DirOrFilePath
ls' root = S.each =<< liftIO (ls root)

lsRecursive' :: FilePath -> Shell_ DirOrFilePath
lsRecursive' root = do --
  paths :: [FilePath] <- liftIO $ map mkPath <$> listDirectory root
  forM_ paths $ \path -> liftIO (doesDirectoryExist path) >>= \case
    True -> S.yield (Left path) >> lsRecursive' path
    False -> S.yield $ Right path
  where
    mkPath name = root <> "/" <> name

-- | List directories & files relative to root
lsRecursive2 :: FilePath -> Shell_ DirOrFilePath
lsRecursive2 root_ = do
  liftIO $ cd root_
  go "." & S.map (bimap dropDotSlash dropDotSlash)
  where
    go root = do
      paths :: [FilePath] <- liftIO $ map mkPath <$> listDirectory root
      forM_ paths $ \path -> liftIO (doesDirectoryExist path) >>= \case
        True -> S.yield (Left path) >> lsRecursive' path
        False -> S.yield $ Right path
      where
        mkPath name = root <> "/" <> name

-- | Drop "./" from a string
dropDotSlash :: String -> String
dropDotSlash xs = fromMaybe xs $ L.stripPrefix "./" xs

-- * Measure time

timeIO :: IO a -> IO (a, Time.NominalDiffTime)
timeIO action = do
  t0 <- Time.getCurrentTime
  a <- action
  t1 <- Time.getCurrentTime
  return (a, Time.diffUTCTime t1 t0)

timePrint :: String -> IO a -> IO a
timePrint label action = do
  (a, time) <- timeIO action
  putStrLn $ label <> " took " <> show time
  return a

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
