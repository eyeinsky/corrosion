{-# LANGUAGE NoImplicitPrelude #-}
module Coreutils where

import Prelude hiding (lines)
import Core

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Streaming.Prelude qualified as S
import System.IO qualified as IO

-- * Handles

-- stdout :: S.Stream (S.Of Buffer) IO () -> S.Stream (S.Of ()) IO ()
-- stdout = S.mapM (BS.hPut IO.stdout . coerce)

stdoutLn :: S.Stream (S.Of Buffer) IO () -> S.Stream (S.Of ()) IO ()
stdoutLn = S.mapM (hPutLn IO.stdout . coerce)

sterr :: S.Stream (S.Of Buffer) IO () -> S.Stream (S.Of ()) IO ()
sterr = S.mapM (BS.hPut IO.stderr . coerce)

sterrLn :: S.Stream (S.Of Buffer) IO () -> S.Stream (S.Of ()) IO ()
sterrLn = S.mapM (hPutLn IO.stderr . coerce)

-- * Conversions

-- | Convert stream of buffers to stream of lines
lines :: S.Stream (S.Of Buffer) IO () -> S.Stream (S.Of Line) IO ()
lines s = S.for s (S.each . coerce @_ @[Line] . BS8.lines . coerce)

-- -- A direct implementation of the above
-- lines :: S.Stream (S.Of Buffer) IO () -> S.Stream (S.Of Line) IO ()
-- lines s = liftIO (S.next s) >>= \case
--   Left _ -> return ()
--   Right (b, s') -> mapM (S.yield . coerce) (BS.lines (coerce b)) *> lines s'

-- * Coreutils

-- ** filesystem =
-- mknod
-- sync
-- mktemp
-- stat
-- chmod
-- chown
-- chgrp
-- cp
-- cat
-- dir
-- ln
-- ls
-- mv

-- pwd
pwd :: MonadIO m => m FilePath
pwd = liftIO getCurrentDirectory

-- readlink
-- realpath
-- rm
-- rmdir
-- basename
-- dirname
-- mkdir
-- mkfifo
-- df
-- shred
-- touch
-- tac
-- du

-- ** Transform, digest, aggregate

-- encoding =
-- basenc
-- b2sum
-- base32
-- base64
-- md5sum
-- sha1sum
-- sha224sum
-- sha256sum
-- sha384sum
-- sha512sum
-- truncate
-- sum

-- | wc
wc_lines :: FilePath -> IO Int
wc_lines f = IO.withFile f IO.ReadMode $ \h -> let
  go n = do
    IO.hIsEOF h >>= \case
      True -> return n
      False -> void (BS.hGetLine h) *> go (n + 1)
  in go 0

-- wc_words = undefined

-- readFile :: FilePath -> (Stream (Of String) IO () -> IO a) -> IO a
-- readFile f s = IO.withFile f IO.ReadMode $ \h -> s (fromHandle h)

-- fromHandle :: MonadIO m => IO.Handle -> Stream (Of String) m ()
-- fromHandle h = go
--   where
--     go = do
--         eof <- liftIO $ IO.hIsEOF h
--         unless eof $ do
--             str <- liftIO $ IO.hGetLine h
--             yield str
--             go

-- process =
-- nice
-- timeout
-- sleep
-- test
-- true
-- kill
-- env
-- yes
-- expr
-- false
-- chroot

-- linux =
-- uname
-- uptime

-- data = -- modify, generate bytes, lines, etc
-- shuf
-- numfmt
-- echo
-- cut
-- fmt
-- head
-- nl
-- od
-- printf
-- tail
-- seq
-- split
-- csplit
-- sort
-- tr
-- uniq
-- paste
-- dd

-- users =
-- id
-- who
-- users
-- whoami




-- list all commands: ls -1 $(dirname $(realpath $(which echo))):
-- '['
-- chcon
-- cksum
-- comm
-- coreutils
-- date
-- dircolors
-- expand
-- factor
-- fold
-- groups
-- hostid
-- install
-- join
-- link
-- logname
-- nohup
-- nproc
-- pathchk
-- pinky
-- pr
-- printenv
-- ptx
-- runcon
-- stdbuf
-- stty
-- tee
-- tsort
-- tty
-- unexpand
-- unlink
-- vdir
