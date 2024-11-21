module Llm.ChatGpt where

import Data.Text (Text, toLower, pack)
import Etc.Context (openAiKey)
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenAI.Client ( ChatCompletionRequest(..), ChatMessage(..), ChatResponse, ModelId(..)
                     , completeChat, makeOpenAIClient
                     )
import Servant.Client.Core.ClientError (ClientError(..))

-- @todo: redo using Reader
chat :: ChatCompletionRequest -> IO (Either ClientError ChatResponse)
chat req = do
  apikey <- openAiKey
  manager <- newManager tlsManagerSettings
  let client = makeOpenAIClient apikey manager retries; retries = 3
  completeChat client req

  -- case cres of
  --   Left  err -> print err
  --   Right res -> print $ chrChoices res

newtype Content = Content { content :: Text }
  deriving (Eq, Generic, Show)

data Role = User | System
  deriving (Eq, Generic, Show)

chatRequest :: Content -> Role -> ChatCompletionRequest
chatRequest (Content c) role =
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
