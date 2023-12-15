{-# LANGUAGE NoImplicitPrelude #-}
module Builtins where

import Prelude
import System.Directory

cd :: FilePath -> IO ()
cd path = setCurrentDirectory path
