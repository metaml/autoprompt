module Llm.ChatGpt where

import Control.Monad.Reader
import Data.Aeson
import Data.Proxy
import Data.Text (Text, toLower, pack)
import Etc.Context (openAiKey)
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenAI.Client
import Servant.API

newtype Content = Content { content :: Text }
  deriving (Eq, Generic, Show)

data Role = User | System
  deriving (Eq, Generic, Show)

chatCompletionRequest :: Content -> Role -> ChatCompletionRequest
chatCompletionRequest (Content c) role =
  ChatCompletionRequest
  { chcrModel = ModelId "gpt-4o"
  , chcrMessages = [ ChatMessage { chmContent = Just c
                                 , chmRole = toLower (pack . show $ role)
                                 , chmFunctionCall = Nothing
                                 , chmName = Nothing
                                 }
                   ]
  , chcrFunctions = Nothing
  , chcrTemperature = Nothing
  , chcrTopP = Nothing
  , chcrN = Nothing
  , chcrStream = Nothing
  , chcrStop = Nothing
  , chcrMaxTokens = Nothing
  , chcrPresencePenalty = Nothing
  , chcrFrequencyPenalty = Nothing
  , chcrLogitBias = Nothing
  , chcrUser = Nothing
  }

post :: IO ()
post = do
  manager <- newManager tlsManagerSettings
  apikey <- openAiKey
  let retries = 3
      client = makeOpenAIClient apikey manager retries
  res <- completeChat client
                      (chatCompletionRequest (Content "Who is the greatest scientist alive?") User)
  case res of
       Left  failure -> print failure
       Right success -> print $ chrChoices success

-- data Message = Message { text :: Text
--                        } deriving (Eq, Generic, Show, ToJSON, FromJSON)

-- -- Buj post works but it's complains witht the following error:
-- -- UnsupportedContentType application/octet-stream (
-- --   Response { responseStatusCode = Status {statusCode = 200, statusMessage = "OK"}
-- --            , responseHeaders = fromList [("Date","Thu, 28 Sep 2023 17:24:16 GMT")
-- --            , ("Content-Length","0"),("Connection","keep-alive")
-- --            , ("X-Content-Type-Options","nosniff")
-- --            , ("X-XSS-Protection","1; mode=block")
-- --            , ("Cache-Control","no-cache, no-store, max-age=0, must-revalidate")
-- --            , ("Pragma","no-cache"),("Expires","0")
-- --            , ("X-Frame-Options","DENY")]
-- --            , responseHttpVersion = HTTP/1.1, responseBody = ""
-- --            }
-- -- )

-- type BujApi = "v1"
--               :> "workspaces"
--               :> "800bf024-9749-401f-aaa4-6f3e563470e1"
--               :> "webhooks"
--               :> "7e198fba-e244-440f-aa89-859caf4d7f38"
--               :> ReqBody '[JSON, PlainText] Message
--               :> Post '[JSON, PlainText] Message

-- bujApi :: Proxy BujApi
-- bujApi = Proxy

-- postMessage :: Message -> ClientM Message

-- postMessage = client bujApi

-- -- goes to the "general" stream
-- postMsg :: Message -> IO ()
-- postMsg msg = do
--   manager' <- newManager tlsManagerSettings
--   r <- runClientM (postMessage msg) (mkClientEnv manager' (BaseUrl Https "api.bujapp.com" 443 ""))
--   case r of
--     Left  err -> putStrLn $ "Error: " ++ show err
--     Right msg' -> print msg'
