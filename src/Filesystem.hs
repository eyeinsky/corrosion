module Filesystem where

import Prelude
import InternalPrelude
import Streaming.Prelude qualified as S
import Data.List qualified as L

import Core


type DirOrFilePath = Either FilePath FilePath
--                          ^ dir    ^ file

-- | List directory content as relative paths
lsPrim :: FilePath -> IO [DirOrFilePath]
lsPrim root = do
  paths :: [FilePath] <- listDirectory root
  forM paths $ \name -> boolEither name name <$> doesDirectoryExist (root <> "/" <> name)

-- | List directory content without checking entry type, shallow
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
  liftIO $ setCurrentDirectory root_
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
