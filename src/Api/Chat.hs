module Api.Chat where

import Api.Model ( ChatRes(..), ChatReq(..), Message(..), MessageRes(..), MessageReq(..)
                 , flatten
                 )
import Control.Monad.State.Strict (liftIO)
-- import Data.Aeson (FromJSON, ToJSON)
import Data.Either (isRight)
import Data.Either.Combinators (fromRight')
import Data.Function ((&))
import Data.Maybe (fromJust, isJust)
-- import Data.Text (Text)
import GHC.Generics (Generic)
import Llm.Model (Content(..), Role(..), chatRequest)
import OpenAI.Client (chchMessage, chmContent, chmRole, chrChoices)
import Servant ( (:>), (:-), Application, JSON, NamedRoutes, Post, Proxy(..), ReqBody
               , serve
               )
import Servant.Server.Internal (AsServerT, Handler)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as N
import qualified Llm.ChatGpt as Chat
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream as S

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
      Message _ _ member' friend' = N.head req.messages

  cs <- liftIO $ S.fromPure (chatReq <$> N.toList req.messages)
                 & flatten
                 & S.mapM (\r -> Chat.chat r member' friend')
                 & S.filter isRight
                 & fmap fromRight'
                 & fmap chrChoices
                 & flatten
                 & fmap chchMessage
                 & S.filter (\msg -> isJust msg.chmContent)
                 & fmap (\msg -> (msg.chmRole, fromJust msg.chmContent))
                 & S.fold (F.toMap fst (F.lmap snd F.toList))
                 & fmap contents

  msgs <- liftIO $ S.fromPure cs
                 & flatten
                 & fmap (\c -> Message { content = c
                                       , role = "User"
                                       , member = member'
                                       , friend = friend'
                                       }
                        )
                 & S.toList

  pure ChatRes { messages = N.fromList msgs
               , friend = friend'
               }

messages' :: MessageReq -> Handler MessageRes
messages' _ = pure MessageRes { name = "hal"
                              , message = "Hello, World!"
                              }
