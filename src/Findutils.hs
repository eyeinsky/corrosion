module Findutils where

import InternalPrelude
import Streaming.Prelude qualified as S
import Options.Applicative qualified as O

import Core
import Filesystem.OsPath qualified as F
import System.Directory.OsPath qualified as F

data Find = Find [FilePath]

parser :: O.Parser Find
parser = Find
  <$> O.many (O.strArgument (O.metavar "PATH"))

parserInfo :: O.ParserInfo Find
parserInfo = simpleParserInfo "find" parser

main :: Find -> IO ()
main (Find paths) = case paths of
  [] -> doPath "."
  _ -> forM_ (map fromString paths) doPath
  where
    doPath path = do
      isDir <- F.doesDirectoryExist path
      if isDir
        then F.lsRecursive path & S.mapM_ (either F.putStrLn F.putStrLn)
        else do
        isFile <- F.doesFileExist path
        if isFile
          then F.putStrLn path
          else throwIO $ userError $ "find: ‘"<> show path <>"’: No such file or directory"
