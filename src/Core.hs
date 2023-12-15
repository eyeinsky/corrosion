{-# LANGUAGE NoImplicitPrelude #-}
module Core
  ( module Core
  , module Export
  ) where

import Prelude as Export hiding ((^), lines)
import Control.Lens as Export hiding ((.=)) -- conflicts with Data.Aeson..=
import Control.Monad.Trans as Export (lift)
import Data.Aeson as Export
import Data.Aeson.Lens as Export
import Data.Char as Export
import Data.Either as Export (partitionEithers)
import Data.Functor as Export
import Data.Maybe as Export
import Data.Scientific as Export
import Data.String as Export (IsString)

import Control.Monad.IO.Class as Export (MonadIO(liftIO))
import Data.Coerce as Export
import System.Directory as Export
import System.Environment as Export
import System.IO as Export
  ( Handle, stdin, stdout, stderr, hIsEOF )
import System.Process as Export

--

import Data.ByteString as BS
import Control.Exception qualified as IO


-- import Streaming.Prelude qualified as S
import Streaming qualified as S


-- * Shell

type Shell e a = S.Stream (S.Of e) IO a
type Shell_ e = Shell e ()

-- * Buffer

newtype Buffer = Buffer ByteString
  deriving stock (Eq, Show)
  deriving newtype (IsString)
newtype Line = Line ByteString
  deriving stock (Eq, Show)
  deriving newtype (IsString)

hPutLn :: Handle -> ByteString -> IO ()
hPutLn h bs = BS.hPut h bs *> BS.hPut h "\n"


-- * Process

readShell :: String -> IO String
readShell str = case words str of
  list@(_cmd : _args) -> do
    let cp = createProcessDefault $ ShellCommand $ unwords list
    readCreateProcess cp ""
  [] -> IO.throwIO $ userError "readShell requires a non empty-string as argument"

createProcessDefault :: CmdSpec -> CreateProcess
createProcessDefault cmdSpec = CreateProcess
  { cmdspec = cmdSpec
  , cwd = Nothing
  , env = Nothing
  , std_in = Inherit
  , std_out = Inherit
  , std_err = Inherit
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  , detach_console = False
  , create_new_console = False
  , new_session = False
  , child_group = Nothing
  , child_user = Nothing
  , use_process_jobs = False }
