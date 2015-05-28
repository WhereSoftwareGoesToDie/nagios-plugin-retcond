module Main where

import           System.Nagios.Plugin         (runNagiosPlugin)
import           System.Nagios.Plugin.Retcond

main :: IO ()
main = runNagiosPlugin checkRetcond
