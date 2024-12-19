module Api.Prompt where

import Api.Model ( ChatRes(..), ChatReq(..), Message(..), MessageReq(..), MessageRes(..)
                 , flatten
                 )
import Control.Monad.State.Strict (liftIO)
import Data.Aeson.Types (parseJSON, parseMaybe)
import Data.Either (isRight)
import Data.Either.Combinators (fromRight')
import Data.Function ((&))
import Data.Maybe (fromJust, isJust)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, pack)
import Data.Text.Conversions (ToText(..))
import Database.PostgreSQL.Simple (Connection)
import Db.Db (connection)
import GHC.Generics (Generic)
import Llm.ChatGpt (systemPrompts, historyPrompts)
import Llm.Model (Content(..), Role(..), chatRequest)
import OpenAI.Client (chchMessage, chmContent, chmRole, chrChoices)
import Servant ( (:>), (:-), Application, JSON, NamedRoutes, Post, Proxy(..), ReqBody
               , serve
               )
import Servant.Server.Internal (AsServerT, Handler)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as N
import qualified Db.Db as Db
import qualified Db.Query as Db
import qualified Llm.ChatGpt as Chat
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream as S

data PromptsApi mode = PromptsApi
  { history :: mode :- "history" :> ReqBody '[JSON] MessageReq :> Post '[JSON] [MessageRes]
  , system  :: mode :- "system"  :> ReqBody '[JSON] MessageReq :> Post '[JSON] ChatRes
  , add     :: mode :- "add"     :> ReqBody '[JSON] ChatReq    :> Post '[JSON] ChatRes
  , update  :: mode :- "udpate"  :> ReqBody '[JSON] ChatReq    :> Post '[JSON] ChatRes
  } deriving Generic

type PromptRoutes = NamedRoutes PromptsApi

promptsApp :: Application
promptsApp = serve (Proxy @PromptRoutes) promptsApi

promptsApi :: PromptsApi  (AsServerT Handler)
promptsApi = PromptsApi
  { history = history'
  , system  = system'
  , add     = add'
  , update  = update'
  }

type MemberId = Text
type FriendId = Text

history' :: MessageReq -> Handler [MessageRes]
history' (MessageReq mid fid) = do
  c <- liftIO Db.connection
  liftIO $ S.fromEffect (historyPrompts c mid fid)
           & flatten
           & fmap (\msg -> if msg.role == "user"
                           then (MessageRes msg.content msg.member)
                           else (MessageRes msg.content msg.member)
                  )
           & S.toList

system' :: MessageReq -> Handler ChatRes
system' (MessageReq mid fid) = do
  liftIO $ print mid
  liftIO $ print fid
  c <- liftIO Db.connection
  msgs <- liftIO $ systemPrompts c mid fid
  pure $ ChatRes (N.fromList msgs) "system"

add' :: ChatReq -> Handler ChatRes
add' req = undefined

update' :: ChatReq -> Handler ChatRes
update' req = undefined
