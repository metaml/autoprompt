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
  { messages :: NonEmpty Message
  , stream   :: Bool
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data ChatRes = ChatRes
  { messages :: NonEmpty Message
  , friend   :: Text -- convenience
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data MessageReq = MessageReq
  { member :: Text
  , friend :: Text
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

data MessageRes = MessageRes
  { name    :: Text
  , message :: Text
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

flatten :: Stream IO [a] -> Stream IO a
flatten = S.concatMap (toStream . fromFoldable)

-- prompts= [ {'content': 'Ask questions in context with the conversation from time to time but not too frequently.'
--            , 'role': 'system'}
--          , {'content': 'Focus on the last reply.'
--            , 'role': 'system'
--            }
--          , {'content': 'Respond with kindness and empathy considering the emotional context and needs of the person. Be supportive language and understanding in your response. Be polite, nice, tell interesting stories once in a while when appropriate, speak naturally, and be less formal, avoid using slang words, and offer insights and opinions.'
--            , 'role': 'system'
--            }
--          ]
