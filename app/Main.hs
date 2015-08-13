module Main where

import Hunch.Options.Data
import Hunch.Options.CommandLine
import Hunch.Runner

import Data.Maybe    (isNothing)
import Control.Monad (when)

main :: IO ()
main = withOptions $ \opts -> do
  let versionOpt  = version opts
      simulateOpt = simulate opts
      noInput     = isNothing . input $ opts

  -- Priority on version flag, then simulation flag.
  -- If no input is given for simulation or default mode, an error
  -- is raised and helptext is shown.
  when versionOpt  $ printVersion opts
  when noInput       showHelpText
  when simulateOpt $ runSimulation opts

  runCreation opts
