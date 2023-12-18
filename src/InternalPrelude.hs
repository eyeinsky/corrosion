module InternalPrelude
  ( module Prelude
  , module InternalPrelude
  , module Export
  ) where

import Data.Coerce as Export
import Control.Monad as Export (forM, forM_, when)
import Control.Monad.IO.Class as Export (MonadIO, liftIO)
import Control.Exception as Export (throwIO)
import Data.String as Export (IsString(fromString))

import Prelude hiding ((^))
import Options.Applicative qualified as O

bool :: a -> a -> Bool -> a
bool a b t = if t then a else b

boolEither :: a -> b -> Bool -> Either a b
boolEither l r t = bool (Left l) (Right r) t

-- * Operators

infixr 9 ^
(^) :: (a -> b) -> (b -> c) -> a -> c
(^) = flip (.)

-- * (,) is (:=)

type (:=) a b = (a, b)

pattern (:=) :: a -> b -> a := b
pattern (:=) a b = (a, b)

infixr 0 :=

{-# COMPLETE (:=) #-}

-- * CLI

simpleCli :: String -> O.Parser a -> IO a
simpleCli header cli = O.execParser $ simpleParserInfo header cli

simpleParserInfo :: String -> O.Parser a -> O.ParserInfo a
simpleParserInfo header cli = O.info (O.helper <*> cli) $ O.fullDesc <> O.header header

headerDescrCli :: String -> String -> O.Parser a -> IO a
headerDescrCli header descr cli = O.execParser $ O.info (O.helper <*> cli) $ O.fullDesc <> O.header header <> O.progDesc descr

-- * Generic

labelPrint :: (MonadIO m, Show a) => String -> a -> m ()
labelPrint label a = liftIO $ putStrLn $ label <> " " <> show a
