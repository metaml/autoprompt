module Model.Api.Chat where

import Control.Monad.State.Strict (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (isRight)
import Data.Either.Combinators (fromRight')
import Data.Function ((&))
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import Llm.ChatGpt (Content(..), Role(..), chatRequest)
import OpenAI.Client (chchMessage, chmContent, chmRole, chrChoices)
import Servant ( (:>), (:-), Application, JSON, NamedRoutes, Post, Proxy(..), ReqBody
               , serve
               )
import Servant.Server.Internal (AsServerT, Handler)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as N
import qualified Llm.ChatGpt as Chat
import qualified Model.Streamly as S
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream as S

data Message = Message
  { content :: Text
  , role :: Text
  , member :: Text
  , friend :: Text
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data ChatReq = ChatReq
  { messages :: N.NonEmpty Message
  , stream :: Bool
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data ChatRes = ChatRes
  { messages :: N.NonEmpty Message
  , friend :: Text
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data MessageReq = MessageReq
  { member :: Text
  , friend :: Text
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data MessageRes = MessageRes
  { name :: Text
  , message :: Text
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data ChatApi mode = ChatApi
  { chat     :: mode :- "chat"     :> ReqBody '[JSON] ChatReq    :> Post '[JSON] ChatRes
  , messages :: mode :- "messages" :> ReqBody '[JSON] MessageReq :> Post '[JSON] MessageRes
  } deriving Generic

type ChatRoutes = NamedRoutes ChatApi

chatApp :: Application
chatApp = serve (Proxy @ChatRoutes) chatApi

chatApi :: ChatApi  (AsServerT Handler)
chatApi = ChatApi
  { chat     = chat'
  , messages = messages'
  }

chat' :: ChatReq -> Handler ChatRes
chat' req = do
  let chatReq = \msg -> chatRequest (Content msg.content) User
      contents = \roleMsgs -> case (M.lookup "assistant" roleMsgs) of
                                Just cs -> cs
                                Nothing -> ["I'm speachless."]
      Message _ _ member friend = N.head req.messages
  cs <- liftIO $ S.fromPure (chatReq <$> N.toList req.messages)
                 & S.flatten
                 & S.mapM Chat.chat
                 & S.filter isRight
                 & fmap fromRight'
                 & fmap chrChoices
                 & S.flatten
                 & fmap chchMessage
                 & S.filter (\msg -> isJust msg.chmContent)
                 & fmap (\msg -> (msg.chmRole, fromJust msg.chmContent))
                 & S.fold (F.toMap fst (F.lmap snd F.toList))
                 & fmap contents
  messages <- liftIO $ S.fromPure cs
                       & S.flatten
                       & fmap (\c -> Message { content = c
                                             , role = "User"
                                             , member = member
                                             , friend = friend
                                             }
                         )
                       & S.toList
  pure ChatRes { messages = N.fromList messages
               , friend = friend
               }

messages' :: MessageReq -> Handler MessageRes
messages' r = pure MessageRes { name = "hal"
                              , message = "Hello, World!"
                              }
