module Llm.Model where

import Data.Text (Text, pack, toLower)
import GHC.Generics (Generic)
import OpenAI.Client (ChatCompletionRequest(..), ChatMessage(..), ModelId(..))

newtype Content = Content { content :: Text }
  deriving (Eq, Generic, Show)

data Role = User | System
  deriving (Eq, Generic, Show)

chatRequest :: Content -> Role -> ChatCompletionRequest
chatRequest (Content c) role =
  ChatCompletionRequest
  { chcrModel = ModelId "gpt-4o"
  , chcrMessages = [ ChatMessage { chmContent = Just c
                                 , chmRole = lowerCase role
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

chatMessage :: Content -> Role -> ChatMessage
chatMessage (Content c) r = ChatMessage { chmContent = Just c
                                        , chmRole = lowerCase r
                                        , chmFunctionCall = Nothing
                                        , chmName = Nothing
                                        }

lowerCase :: Role -> Text
lowerCase = toLower . pack . show
