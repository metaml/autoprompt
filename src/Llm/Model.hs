module Llm.Model where

import Data.Aeson ( FromJSON(..), ToJSON(..)
                  , defaultOptions, constructorTagModifier, genericParseJSON, genericToJSON
                  )
import Data.Strings (strToLower)
import Data.Text (Text, pack, toLower)
import Data.Text.Conversions (ToText(..))
import GHC.Generics (Generic)
import OpenAI.Client (ChatCompletionRequest(..), ChatMessage(..), ModelId(..))

data Message = Message { content :: Content, role :: Role }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype Content = Content Text
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving newtype ToText

data Role = Assistant | User | System
  deriving (Eq, Generic, Show)

instance FromJSON Role where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = strToLower }

instance ToJSON Role where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = strToLower }

chatRequest :: Content -> Role -> ChatCompletionRequest
chatRequest c r = chatRequest' [Message c r]

chatRequest' :: [Message] -> ChatCompletionRequest
chatRequest' messages =
  ChatCompletionRequest { chcrModel = ModelId "gpt-4o"
                        , chcrMessages = chatMessage <$> messages
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

chatMessage :: Message -> ChatMessage
chatMessage (Message (Content c) r) =
  ChatMessage { chmContent = Just c
              , chmRole    = lowercase r
              , chmFunctionCall = Nothing
              , chmName         = Nothing
              }
  where lowercase :: Role -> Text
        lowercase = toLower . pack . show

promptSys :: Content -> Message
promptSys c = Message c System

promptUser :: Content -> Message
promptUser c = Message c User
