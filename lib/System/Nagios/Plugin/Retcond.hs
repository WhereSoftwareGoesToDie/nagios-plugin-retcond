{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Nagios.Plugin.Retcond where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text.Lens
import Data.Int
import Control.Lens hiding ((.=))
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
  { _sourceNumNotifications :: Int64
  , _sourceNumKeys          :: Int64
  } deriving (Eq, Show)

makeLenses ''DataSourceMeters

instance FromJSON DataSourceMeters where
    parseJSON (Object o) = DataSourceMeters <$> o .: "gauge_notifications"
                                            <*> o .: "gauge_keys"
    parseJSON _          = fail "DataSourceMeters must be an object"


renderDataSourceMeters :: Text
                       -> Text
                       -> DataSourceMeters
                       -> [PerfDatum]
renderDataSourceMeters entity source dsm =
    let prefix = "source_" <> entity <> "_" <> source <> "_" in
        [ barePerfDatum (prefix <> "notifications")
                        (IntegralValue (dsm ^. sourceNumNotifications))
                        NullUnit
        , barePerfDatum (prefix <> "keys")
                        (IntegralValue (dsm ^. sourceNumKeys))
                        NullUnit
        ]

data EntityMeters = EntityMeters
  { _entityNumNotifications :: Int64
  , _entityNumCreates       :: Int64
  , _entityNumUpdates       :: Int64
  , _entityNumDeletes       :: Int64
  , _entityNumConflicts     :: Int64
  , _entityNumKeys          :: Int64
  , _entityDataSourceMeters :: Map Text DataSourceMeters
  } deriving (Eq, Show)

makeLenses ''EntityMeters

instance FromJSON EntityMeters where
    parseJSON (Object o) = EntityMeters <$> o .: "gauge_notifications"
                                        <*> o .: "counter_creates"
                                        <*> o .: "counter_updates"
                                        <*> o .: "counter_deletes"
                                        <*> o .: "gauge_conflicts"
                                        <*> o .: "gauge_keys"
                                        <*> o .: "datasources"
    parseJSON _          = fail "DataSourceMeters must be an object"

renderEntityMeters :: Text
                   -> EntityMeters
                   -> [PerfDatum]
renderEntityMeters entity em =
    let prefix = "entity_" <> entity <> "_" in
        [ barePerfDatum (prefix <> "notifications")
                        (IntegralValue (em ^. entityNumNotifications))
                        NullUnit
        , barePerfDatum (prefix <> "creates")
                        (IntegralValue (em ^. entityNumCreates))
                        Counter
        , barePerfDatum (prefix <> "updates")
                        (IntegralValue (em ^. entityNumUpdates))
                        Counter
        , barePerfDatum (prefix <> "deletes")
                        (IntegralValue (em ^. entityNumDeletes))
                        Counter
        , barePerfDatum (prefix <> "conflicts")
                        (IntegralValue (em ^. entityNumConflicts))
                        Counter
        , barePerfDatum (prefix <> "keys")
                        (IntegralValue (em ^. entityNumKeys))
                        NullUnit
        ]

data RetconMeters = RetconMeters
  { _entityMeters           :: Map Text EntityMeters
  , _serverNumNotifications :: Int64
  } deriving (Eq, Show)

makeLenses ''RetconMeters

instance FromJSON RetconMeters where
    parseJSON (Object o) = RetconMeters <$> o .: "datasources"
                                        <*> o .: "gauge_notifications"
    parseJSON _          = fail "RetconMeters must be an object"

instance ToPerfData RetconMeters where
    toPerfData rm =
        let base = [ barePerfDatum "notifications"
                                   (IntegralValue (rm ^. serverNumNotifications))
                                   NullUnit
                   ]
            entities = concat . map (uncurry renderEntityMeters) $ M.assocs (rm ^. entityMeters) in
        base <> entities

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
    checkRetcond' bs = case (decode bs :: Maybe RetconMeters) of
        Nothing -> addResult Critical "failed to parse ekg output"
        Just meters -> do
            addPerfData meters
            addResult OK "perfdata only"