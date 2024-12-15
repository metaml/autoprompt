module Llm.ChatGpt where

import Api.Model (flatten)
import Data.Aeson (FromJSON, Value, decode, parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Aeson.Text (encodeToLazyText)
import Data.Maybe (fromJust, isJust)
import Db.Db (connection)
import Db.Entity.Conversation (ConversationT(..))
import Db.Entity.Prompt (PromptT(..))
import Db.Query (history, prompts)
import Data.Function ((&))
import Data.Text (Text, toLower, pack)
import Data.Text.Conversions (ToText(..))
import Database.PostgreSQL.Simple (Connection)
import Etc.Context (openAiKey)
import GHC.Generics (Generic)
import Llm.Model ( Content(..), Message(..),
                   content, promptSys
                 )
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenAI.Client ( ChatCompletionRequest(..), ChatMessage(..), ChatResponse, ModelId(..)
                     , completeChat, makeOpenAIClient
                     )
import Servant.Client.Core.ClientError (ClientError(..))
import qualified Api.Model as Api
import qualified Data.Map as M
import qualified Data.List.NonEmpty as N
import qualified Llm.Model as Llm
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream as S

type MemberId = Text
type FriendId = Text

-- @todo: redo using Reader
chat :: ChatCompletionRequest -> MemberId -> FriendId -> IO (Either ClientError ChatResponse)
chat req mid fid = do
  db <- connection
  sysPrompts <- systemPrompts db mid fid
  histPrompts <- historyPrompts db mid fid
  messages <- S.fromPure (sysPrompts <> histPrompts)
              & flatten
              & fmap (\p -> ChatMessage (Just p.content) p.role Nothing Nothing)
              & S.toList

  key <- openAiKey
  mgr <- newManager tlsManagerSettings
  let req'   = req{ chcrMessages = (messages <> req.chcrMessages) }
      openai = makeOpenAIClient key mgr 3 -- 3 retries

  completeChat openai req'

systemPrompts :: Connection ->  MemberId -> FriendId -> IO [Api.Message]
systemPrompts db mid fid = S.fromEffect (prompts db "system")
                           & flatten
                           & fmap (\p -> p.promptPrompt)
                           & fmap (\p -> promptSys (Content p))
                           & fmap (\msg -> toText msg.content)
                           & fmap (\content -> Api.Message content "system" mid fid)
                           & S.toList

historyPrompts :: Connection ->  MemberId -> FriendId -> IO [Api.Message]
historyPrompts db mid fid = S.fromEffect (history db mid fid)
                            & flatten
                            & fmap (\(v, mid, fid) -> ( parseMaybe parseJSON v :: Maybe Message
                                                      , mid
                                                      , fid
                                                      )
                                )
                            & S.filter (\(msg, _, _) -> isJust msg)
                            & fmap (\(msg, mid, fid) -> (fromJust msg, mid, fid))
                            & fmap (\(msg, mid, fid) -> (toText msg.content, mid, fid))
                            & fmap (\(content, mid, fid) -> Api.Message content "system" mid fid)
                            & S.toList
