{-# LANGUAGE RecordWildcards #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Nagios.Plugin.Retcond where

import Control.Lens
import Control.Applicative
import Data.Aeson
import Network.Wreq
import System.Nagios.Plugin
import Options.Applicative       

data CheckOpts = CheckOpts
  { checkEkgEndpoint :: String }

checkOptParser :: ParserInfo CheckOpts
checkOptParser =  info (helper <*> opts)
              (   fullDesc
               <> progDesc "Nagios check for retcond."
               <> header   "nagios-plugin-retcond"
              )
  where
    opts = CheckOpts
           <$> strOption (   long "ekg-endpoint"
                          <> short 'e'
                          <> metavar "EKG-ENDPOINT"
                          <> value "http://localhost:8888"
                          <> help "URI of the ekg endpoint to check."
                         )
       
checkRetcond :: NagiosPlugin ()
checkRetcond = do
    CheckOpts{..} <- liftIO $ execParser checkOptParser
    let reqOpts = defaults & header "Accept" .~ ["application/json"]
    resp <- liftIO $ getWith reqOpts checkEkgEndpoint
    case resp ^. responseStatus . statusCode of
         200  -> checkRetcond' $ resp ^. responseBody
         code -> addResult Critical $ "ekg endpoint returned " <> show code
  where
    checkRetcond' bs = undefined
