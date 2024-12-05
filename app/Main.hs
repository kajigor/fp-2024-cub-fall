{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import MyRegex (eval)
import Parser (parse)
import MainPage (mainPage)
import MyDebugger (debugEval)

data ClientMessage = ClientMessage
    { regex :: Text
    , input :: Text
    } deriving (Show, Generic)

data DebugResponse = DebugResponse
    { isMatch :: Bool
    , debugSteps :: [String]
    } deriving (Show, Generic)

instance FromJSON ClientMessage
instance ToJSON DebugResponse

wsApp :: WS.ServerApp
wsApp pending = do
    conn <- WS.acceptRequest pending
    WS.sendTextData conn ("Connected to WebSocket server!" :: Text)
    forever $ do
        msg <- WS.receiveData conn :: IO B.ByteString
        case eitherDecode msg of
            Left _ -> WS.sendTextData conn ("<div style=\"color:red;\">Invalid JSON format</div>" :: Text)
            Right (ClientMessage rgx inputText) ->
                if T.null rgx
                then WS.sendTextData conn ("<div style=\"color:red;\">Empty regex provided</div>" :: Text)
                else case parse (T.unpack rgx) of
                    Left parseError ->
                        WS.sendTextData conn ("<div style=\"color:red;\">Invalid regex: " <> T.pack (show parseError) <> "</div>")
                    Right compiledRegex -> do
                        let matched = eval compiledRegex (T.unpack inputText)
                        let dSteps = map show $ debugEval compiledRegex (T.unpack inputText)
                        let response = DebugResponse matched dSteps
                        WS.sendTextData conn (encode response)

httpApp :: Application
httpApp _ respond = do
    respond $ responseLBS status200 [("Content-Type", "text/html")] mainPage

main :: IO ()
main = do
    putStrLn "Server running at http://localhost:8080"
    let app = websocketsOr WS.defaultConnectionOptions wsApp httpApp
    run 8080 app