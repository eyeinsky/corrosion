module Findutils where

import InternalPrelude
import Streaming.Prelude qualified as S
import Options.Applicative qualified as O

import Core
import Filesystem.OsPath qualified as F
import System.Directory.OsPath qualified as F

data Variant = Original | New | StraightPaths | Dirent | Dirent2 | Dirent3_readdir
data Find = Find [FilePath] Variant

parser :: O.Parser Find
parser = Find
  <$> O.many (O.strArgument (O.metavar "PATH"))
  <*> O.option (O.eitherReader $ \case
                   "new" -> Right New
                   "paths" -> Right StraightPaths
                   "dirent" -> Right Dirent
                   "dirent2" -> Right Dirent2
                   "dirent3_readdir" -> Right Dirent3_readdir
                   str -> Left $ "Unrecognized variant " <> show str
                   ) (O.long "variant" <> O.value Original)

parserInfo :: O.ParserInfo Find
parserInfo = simpleParserInfo "find" parser

main :: Find -> IO ()
main (Find paths variant) = case paths of
  [] -> go "."
  _ -> forM_ (map fromString paths) go
  where
    go = case variant of
      Original -> goPrim F.lsRecursive (either F.putOsPathLn F.putOsPathLn)
      New -> goPrim F.lsRecursive2 (either F.putOsPathLn F.putOsPathLn)
      StraightPaths -> goPrim F.lsRecursive2_straight F.putOsPathLn
      Dirent -> goPrim F.lsRecursive3_dirent F.putOsPathLn
      Dirent2 -> goPrim F.lsRecursive3_dirent2 F.putOsPathLn
      Dirent3_readdir -> goPrim F.lsRecursive3_dirent3 F.putOsPathLn

    goPrim listRecursive printer path = do
      isDir <- F.doesDirectoryExist path
      if isDir
        then listRecursive path & S.mapM_ printer
        else do
        isFile <- F.doesFileExist path
        if isFile
          then F.putOsPathLn path
          else throwIO $ userError $ "find: ‘"<> show path <>"’: No such file or directory"
