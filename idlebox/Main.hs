module Main where

import InternalPrelude
import Findutils qualified
import Options.Applicative qualified as O


data Command
  = Find Findutils.Find

commands :: O.Parser Command
commands = O.subparser $ O.command "find" (Find <$> Findutils.parserInfo)

main :: IO ()
main = simpleCli "idlebox" commands >>= \case
  Find find -> Findutils.main find
