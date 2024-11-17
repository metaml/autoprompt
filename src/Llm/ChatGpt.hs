module Llm.ChatGpt where

import Data.Text (Text, toLower, pack)
import Etc.Context (openAiKey)
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenAI.Client ( ChatCompletionRequest(..), ChatMessage(..), ModelId(..)
                     , chrChoices, completeChat, makeOpenAIClient
                     )

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

-- @todo: redo using Reader
post :: ChatCompletionRequest -> IO ()
post req = do
  apikey <- openAiKey
  manager <- newManager tlsManagerSettings
  let retries = 3
      client = makeOpenAIClient apikey manager retries
  res <- completeChat client req
  case res of
       Left  failure -> print failure
       Right success -> print $ chrChoices success
