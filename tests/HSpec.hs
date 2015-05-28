{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy         as BS
import qualified Data.Text                    as T
import           Test.Hspec
import           Test.HUnit.Base

import           System.Nagios.Plugin
import           System.Nagios.Plugin.Retcond

main :: IO ()
main = hspec suite

suite :: Spec
suite =
    describe "checkRetcond'" $ do
        validJson <- runIO $ BS.readFile "tests/testdata.json"
        (_, validState) <- runIO . runNagiosPlugin' $ checkRetcond' validJson
        let (validStatus, validOutput) = finishState validState

        it "exits successfully given valid input" $
            validStatus @?= OK
        it "outputs the correct perfdata" $
            validOutput @?= "OK: perfdata only | notifications=2394562348956.0;;;; entity_aleph_notifications=23.0;;;; entity_aleph_creates=0c;;;; entity_aleph_updates=923457c;;;; entity_aleph_deletes=2345c;;;; entity_aleph_conflicts=2.0;;;; entity_aleph_internal_keys=1234.0;;;; source_aleph_beth_notifications=235.0;;;; source_aleph_beth_foreign_keys=567.0;;;; source_aleph_gimel_notifications=2345.0;;;; source_aleph_gimel_foreign_keys=235123.0;;;;"
