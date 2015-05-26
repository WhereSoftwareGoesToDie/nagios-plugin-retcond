{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Nagios.Plugin.Retcond where

import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Lens
import Control.Lens
import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Network.Wreq
import System.Nagios.Plugin
import Options.Applicative       hiding (header)
import qualified Options.Applicative as O

data CheckOpts = CheckOpts
  { checkEkgEndpoint :: String }

data DataSourceMeters = DataSourceMeters
  { sourceNumNotifications :: Integer
  , sourceNumKeys          :: Integer
  }

data EntityMeters = EntityMeters
  { entityNumNotifications :: Integer
  , entityNumCreates       :: Integer
  , entityNumUpdates       :: Integer
  , entityNumDeletes       :: Integer
  , entityNumConflicts     :: Integer
  , entityNumKeys          :: Integer
  , entityDataSourceMeters :: Map Text DataSourceMeters
  }

data RetconMeters = RetconMeters
  { entityMeters           :: Map Text EntityMeters
  , serverNumNotifications :: Integer
  }

checkOptParser :: ParserInfo CheckOpts
checkOptParser =  info (helper <*> opts)
              (   fullDesc
               <> progDesc "Nagios check for retcond."
               <> O.header   "nagios-plugin-retcond"
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
    let reqOpts = header "Accept" .~ ["application/json"] $ defaults
    resp <- liftIO $ getWith reqOpts checkEkgEndpoint
    case resp ^. responseStatus . statusCode of
         200  -> checkRetcond' $ resp ^. responseBody
         code -> addResult Critical $ "ekg endpoint returned " <> (show code ^. packed)
  where
    checkRetcond' bs = undefined
