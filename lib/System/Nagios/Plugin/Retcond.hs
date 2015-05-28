{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.Nagios.Plugin.Retcond where

import           Control.Applicative
import           Control.Lens           hiding ((.=))
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Int
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Text              (Text)
import           Data.Text.Lens
import           Network.Wreq
import           Options.Applicative    hiding (header)
import qualified Options.Applicative    as O
import           System.Nagios.Plugin

data CheckOpts = CheckOpts
  { checkEkgEndpoint :: String }

-- I wish ekg exported this. DRY yo.
data EkgMetricType =
      CounterType
    | GaugeType
    | LabelType
    | DistributionType
  deriving (Eq, Show)

-- Distribution and label not yet implemented.
instance FromJSON EkgMetricType where
    parseJSON (String s) = case s of
        "c" -> return CounterType
        "g" -> return GaugeType
        x   -> fail $ "Invalid metric type " <> (x ^. unpacked)
    parseJSON _          = fail "EkgMetricType must be a string"

newtype EkgCounterValue = EkgCounterValue { _counterValue :: Int64 }
    deriving (Eq, Show)

makeLenses ''EkgCounterValue

instance FromJSON EkgCounterValue where
    parseJSON (Object o) = do
        mt <- o .: "type"
        case mt of
            CounterType -> EkgCounterValue <$> o .: "val"
            x           -> fail $ show x <> " is an invalid metric type for EkgCounterValue"
    parseJSON _          = fail "EkgCounterValue must be an object"

renderCounter :: Text
              -> EkgCounterValue
              -> PerfDatum
renderCounter lbl val = barePerfDatum lbl (IntegralValue (val ^. counterValue)) Counter

newtype EkgGaugeValue = EkgGaugeValue { _gaugeValue :: Double }
    deriving (Eq, Show)

makeLenses ''EkgGaugeValue

instance FromJSON EkgGaugeValue where
    parseJSON (Object o) = do
        mt <- o .: "type"
        case mt of
            GaugeType -> EkgGaugeValue <$> o .: "val"
            x           -> fail $ show x <> " is an invalid metric type for EkgGaugeValue"
    parseJSON _          = fail "EkgGaugeValue must be an object"

renderGauge :: Text
              -> EkgGaugeValue
              -> PerfDatum
renderGauge lbl val = barePerfDatum lbl (RealValue (val ^. gaugeValue)) NullUnit

data DataSourceMeters = DataSourceMeters
  { _sourceNumNotifications :: EkgGaugeValue
  , _sourceNumKeys          :: EkgGaugeValue
  } deriving (Eq, Show)

makeLenses ''DataSourceMeters

instance FromJSON DataSourceMeters where
    parseJSON (Object o) = DataSourceMeters <$> o .: "notifications"
                                            <*> o .: "keys"
    parseJSON _          = fail "DataSourceMeters must be an object"


renderDataSourceMeters :: Text
                       -> Text
                       -> DataSourceMeters
                       -> [PerfDatum]
renderDataSourceMeters entity source dsm =
    let prefix = "source_" <> entity <> "_" <> source <> "_" in
        [ renderGauge (prefix <> "notifications") (dsm ^. sourceNumNotifications)
        , renderGauge (prefix <> "keys") (dsm ^. sourceNumKeys)
        ]

data EntityMeters = EntityMeters
  { _entityNumNotifications :: EkgGaugeValue
  , _entityNumCreates       :: EkgCounterValue
  , _entityNumUpdates       :: EkgCounterValue
  , _entityNumDeletes       :: EkgCounterValue
  , _entityNumConflicts     :: EkgGaugeValue
  , _entityNumKeys          :: EkgGaugeValue
  , _entityDataSourceMeters :: Map Text DataSourceMeters
  } deriving (Eq, Show)

makeLenses ''EntityMeters

instance FromJSON EntityMeters where
    parseJSON (Object o) = EntityMeters <$> o .: "notifications"
                                        <*> o .: "creates"
                                        <*> o .: "updates"
                                        <*> o .: "deletes"
                                        <*> o .: "conflicts"
                                        <*> o .: "keys"
                                        <*> o .: "datasources"
    parseJSON _          = fail "DataSourceMeters must be an object"

renderEntityMeters :: Text
                   -> EntityMeters
                   -> [PerfDatum]
renderEntityMeters entity em =
    let prefix = "entity_" <> entity <> "_" in
        [ renderGauge (prefix <> "notifications") (em ^. entityNumNotifications)
        , renderCounter (prefix <> "creates") (em ^. entityNumCreates)
        , renderCounter (prefix <> "updates") (em ^. entityNumUpdates)
        , renderCounter (prefix <> "deletes") (em ^. entityNumDeletes)
        , renderGauge (prefix <> "conflicts") (em ^. entityNumConflicts)
        , renderGauge (prefix <> "keys") (em ^. entityNumKeys)
        ]

data RetconMeters = RetconMeters
  { _entityMeters           :: Map Text EntityMeters
  , _serverNumNotifications :: EkgGaugeValue
  } deriving (Eq, Show)

makeLenses ''RetconMeters

instance FromJSON RetconMeters where
    parseJSON (Object o) = RetconMeters <$> o .: "entities"
                                        <*> o .: "notifications"
    parseJSON _          = fail "RetconMeters must be an object"

instance ToPerfData RetconMeters where
    toPerfData rm =
        let base = [ renderGauge "notifications" (rm ^. serverNumNotifications) ]
            entities = concatMap (uncurry renderEntityMeters) $ M.assocs (rm ^. entityMeters) in
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
