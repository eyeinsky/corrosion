module Filesystem.OsPath where

import InternalPrelude
import Streaming.Prelude qualified as S

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as BSS
import System.Directory.OsPath qualified as D
import System.OsPath (OsPath, encodeUtf, (</>))
import System.OsString.Internal.Types (OsString(OsString), PosixString(PosixString))

import Core

type DirOrFilePath' = Either OsPath OsPath

-- | List directories & files relative to root
lsRecursive :: OsPath -> Shell_ DirOrFilePath'
lsRecursive root_ = do
  liftIO (D.setCurrentDirectory root_)
  go "."
  where
  go root = do
    names :: [OsPath] <- liftIO $ D.listDirectory root
    forM_ names $ \name -> let
      path = root System.OsPath.</> name
      in liftIO (D.doesDirectoryExist path) >>= \case
        True -> S.yield (Left path) >> go path
        False -> S.yield $ Right path

instance IsString OsPath where
  fromString = either (error . show) id . System.OsPath.encodeUtf

putStrLn :: OsPath -> IO ()
putStrLn p = BS8.putStrLn $ BSS.fromShort sbs
  where
    sbs = coerce @_ @BSS.ShortByteString (coerce @_ @PosixString p)
