{-# OPTIONS_GHC -Wno-orphans #-}
module Filesystem.OsPath where

import InternalPrelude
import Streaming.Prelude qualified as S

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as BSS
import System.Directory.OsPath qualified as D
import System.OsPath (OsPath, encodeUtf, (</>))
import System.OsString.Internal.Types (OsString(OsString), PosixString(PosixString))

import Core

-- new
import System.Posix.Directory.PosixPath qualified as Posix
import Control.Exception qualified as E
import System.OsPath.Types (PosixPath)


type DirOrFilePath' = Either OsPath OsPath

-- | List directories & files relative to root
lsRecursive :: OsPath -> Shell_ DirOrFilePath'
lsRecursive root = do
  names :: [OsPath] <- liftIO $ D.listDirectory root
  forM_ names $ \name -> let
    path = root System.OsPath.</> name
    in do
    when (path == "/nix/store/1xc9kjmqyv4b6khk329qh6rk60152w1p-etc/etc/cups/ssl") $ labelPrint "FOUND THIS PATH 1" ()
    liftIO (D.doesDirectoryExist path) >>= \case
      True -> S.yield (Left path) >> lsRecursive path
      False -> S.yield $ Right path

instance IsString OsPath where
  fromString = either (error . show) id . System.OsPath.encodeUtf

instance IsString PosixPath where
  fromString str = coerce (fromString str :: BSS.ShortByteString)

putOsPathLn :: OsPath -> IO ()
putOsPathLn p = BS8.putStrLn $ BSS.fromShort $ coerce p

-- new

getDirectoryContentsInternal2 :: OsPath -> IO [Posix.PosixFile]
getDirectoryContentsInternal2 path = withDirStream start path
  where
  start dirp = loop id
    where
      loop :: ([Posix.PosixFile] -> [Posix.PosixFile]) -> IO [Posix.PosixFile]
      loop acc = Posix.readDirStream2 dirp >>= \case
        Just e -> loop (acc . (e :))
        Nothing -> pure (acc [])

-- Either streams
lsRecursive2 :: OsPath -> Shell_ DirOrFilePath'
lsRecursive2 dirPath = do
  names :: [Posix.PosixFile] <- filter (not . isSpecial) <$> liftIO (getDirectoryContentsInternal2 dirPath)
  forM_ names $ \(Posix.PosixFile name type_) -> let
    path = dirPath System.OsPath.</> coerce name
    in case type_ of
    Posix.Dir -> S.yield (Left path) >> lsRecursive2 path
    _ -> S.yield $ Right path

-- Direct OsPath stream
lsRecursive2_straight :: OsPath -> Shell_ OsPath
lsRecursive2_straight dirPath = do
  names :: [Posix.PosixFile] <- filter (not . isSpecial) <$> liftIO (getDirectoryContentsInternal2 dirPath)
  forM_ names $ \(Posix.PosixFile name type_) -> let
    path = dirPath System.OsPath.</> coerce name
    in S.yield path *> case type_ of
    Posix.Dir -> lsRecursive2_straight path
    _ -> pure ()

-- Read the dirent struct directly
getDirectoryContentsInternal3 :: OsPath -> IO [Posix.PosixFile]
getDirectoryContentsInternal3 path = withDirStream go path
  where
    go dirp = loop id
      where
        loop :: ([Posix.PosixFile] -> [Posix.PosixFile]) -> IO [Posix.PosixFile]
        loop acc = Posix.readDirStream3 dirp >>= \case
          Just e -> loop (acc . (e :))
          Nothing -> pure (acc [])

lsRecursive3_dirent :: OsPath -> Shell_ OsPath
lsRecursive3_dirent dirPath = do
  names :: [Posix.PosixFile] <- filter (not . isSpecial) <$> liftIO (getDirectoryContentsInternal3 dirPath)
  forM_ names $ \(Posix.PosixFile name type_) -> let
    path = dirPath System.OsPath.</> coerce name
    in S.yield path *> case type_ of
    Posix.Dir -> lsRecursive3_dirent path
    _ -> pure ()

lsRecursive3_dirent2 :: OsPath -> Shell_ OsPath
lsRecursive3_dirent2 dirPath = do
  names <- filter (not . isSpecial) <$> liftIO (withDirStream Posix.readDirStream4 dirPath)
  forM_ names $ \(Posix.PosixFile name type_) -> let
    path = dirPath System.OsPath.</> coerce name
    in S.yield path *> case type_ of
    Posix.Dir -> lsRecursive3_dirent2 path
    _ -> pure ()

lsRecursive3_dirent3 :: OsPath -> Shell_ OsPath
lsRecursive3_dirent3 dirPath = do
  names <- filter (not . isSpecial) <$> liftIO (withDirStream Posix.readDirStream5 dirPath)
  forM_ names $ \(Posix.PosixFile name type_) -> let
    path = dirPath System.OsPath.</> coerce name
    in do
    labelPrint "doing" path
    case type_ of
      Posix.Dir -> S.yield path *> lsRecursive3_dirent3 path
      _ -> S.yield path

hot = lsRecursive3_dirent3 "/nix/store/1xc9kjmqyv4b6khk329qh6rk60152w1p-etc"
  & S.mapM_ print


-- * Helpers

isSpecial :: Posix.PosixFile -> Bool
isSpecial (Posix.PosixFile name _type) = name == "." || name == ".."

withDirStream :: (Posix.DirStream -> IO a) -> OsString -> IO a
withDirStream readDirVariant (OsString path) = E.bracket
  (Posix.openDirStream path)
  Posix.closeDirStream
  readDirVariant
{-# INLINE withDirStream #-}
