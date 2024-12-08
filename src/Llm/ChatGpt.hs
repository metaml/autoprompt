module Llm.ChatGpt where

import Db.Db (connection)
import Db.Query (history, prompts)
import Data.Function ((&))
import Data.Text (Text, toLower, pack)
import Etc.Context (openAiKey)
import GHC.Generics (Generic)
import Llm.Model
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenAI.Client ( ChatCompletionRequest(..), ChatMessage(..), ChatResponse, ModelId(..)
                     , completeChat, makeOpenAIClient
                     )
import Servant.Client.Core.ClientError (ClientError(..))
import qualified Data.Map as M
import qualified Data.List.NonEmpty as N
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream as S

type MemberId = Text
type FriendId = Text

-- @todo: redo using Reader
chat :: ChatCompletionRequest -> MemberId -> FriendId -> IO (Either ClientError ChatResponse)
chat req mid fid = do
  c <- connection
  req' <- S.fromPure (c, req)
          & S.mapM ( \(c, r) -> do
                       ps <- prompts c "system"
                       vs <- history c mid fid
                       pure $ (c, r, ps, vs)
                   )
          & S.toList
  completeChat undefined undefined
  -- prompts <- undefined
  -- history <- undefined

  -- apikey <- openAiKey
  -- manager <- newManager tlsManagerSettings
  -- let client = makeOpenAIClient apikey manager retries; retries = 3
  -- completeChat client req
