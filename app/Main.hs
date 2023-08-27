{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Data.Time ( Day )
import GHC.Generics ( Generic )
import Data.Text ( Text )
import Control.Applicative (empty)
import Data.Aeson.Types
    ( (.:), FromJSON(parseJSON), Value(Object), Parser, ToJSON (toJSON), KeyValue ((.=)), object )
import Network.HTTP.Conduit
import Data.Aeson (encode)
import Network.HTTP.Types.Status

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    let requestObject = encode (LogMessage {message = ""} )
    initialRequest <- parseRequest seqUrl
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS requestObject
            , requestHeaders = [("Content-Type", "application/vnd.serilog.clef")]}
    response <- httpLbs request manager
    putStrLn $ "StatusCode: " ++ show (statusCode $ responseStatus response)

seqUrl :: String
seqUrl = "http://home:5341/api/events/raw?clef"

data LogMessage = LogMessage
    { message :: Text
    , messageTemplate :: Text
    , level :: Text
    , exception :: Text
    , eventId :: Text
    , renderings :: Text
    , timestamp :: Day
    } deriving (Generic, Show)

instance FromJSON LogMessage where
    parseJSON :: Value -> Parser LogMessage
    parseJSON (Object v) = LogMessage
        <$> v .: "@m"
        <*> v .: "@mt"
        <*> v .: "@l"
        <*> v .: "@x"
        <*> v .: "@i"
        <*> v .: "@r"
        <*> v .: "@t"
    parseJSON _ = empty

instance ToJSON LogMessage where
    toJSON :: LogMessage -> Value
    toJSON (LogMessage {..}) = object
        [ "@m" .= message
        , "@mt" .= messageTemplate
        , "@l" .= level
        , "@x" .= exception
        , "@i" .= eventId
        , "@r" .= renderings
        , "@t" .= timestamp]