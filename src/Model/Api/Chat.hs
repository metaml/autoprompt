module Model.Api.Chat where

import Control.Monad.State.Strict (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Text (Text)
import GHC.Generics (Generic)
import Llm.ChatGpt (Content(..), Role(..), chat, chatRequest)
import Servant ( (:>), (:-), Application, Get, JSON, NamedRoutes, Post, Proxy(..), ReqBody
               , serve
               )
import Servant.Server.Internal (AsServerT, Handler)

data Message = Message
  { content :: Text
  , role :: Text
  , member :: Text
  , friend :: Text
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data ChatReq = ChatReq
  { messages :: [Message]
  , stream :: Bool
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data ChatRes = ChatRes
  { messages :: [Message]
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
chat' r = do
  let req = \msg -> chatRequest (Content msg.content) User
      reqs = req <$> r.messages
  ress <- liftIO $ mapM chat reqs
  pure undefined

messages' :: MessageReq -> Handler MessageRes
messages' = undefined
