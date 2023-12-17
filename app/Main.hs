module Main where

import InternalPrelude
import Options.Applicative qualified as O
import Corrosion

main :: IO ()
main = do
  path <- simpleCli "corrode -- wc -l" (O.strArgument O.idm)
  print =<< wc_lines path
