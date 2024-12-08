module Api.Model where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
-- import Servant ((:>), (:-), JSON, Post, ReqBody)
import Streamly.Data.Stream (Stream)
import Streamly.Data.StreamK (fromFoldable, toStream)
import qualified Streamly.Data.Stream as S

data Message = Message
  { content :: Text
  , role :: Text
  , member :: Text
  , friend :: Text
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data ChatReq = ChatReq
  { messages ::NonEmpty Message
  , stream :: Bool
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data ChatRes = ChatRes
  { messages :: NonEmpty Message
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

flatten :: Stream IO [a] -> Stream IO a
flatten = S.concatMap (toStream . fromFoldable)
