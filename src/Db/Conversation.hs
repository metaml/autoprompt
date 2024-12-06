module Db.Conversation where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam (Identity)
import Database.Beam.Schema (Beamable, Columnar, PrimaryKey(..), Table)
import Db.Enum (MessageEnum)
import GHC.Generics (Generic)

data ConversationT f = Conversation { conversationId           :: Columnar f Int
                                    , conversationGuid         :: Columnar f UUID
                                    , conversationMemberId     :: Columnar f Text
                                    , conversationFriendId     :: Columnar f Text
                                    , conversationSpeakerType  :: Columnar f Text
                                    , conversationLine         :: Columnar f Text
                                    , conversationMessage      :: Columnar f Value
                                    , conversationMessageState :: Columnar f MessageEnum
                                    , conversationCreatedAt    :: Columnar f UTCTime
                                    } deriving (Beamable, Generic)

instance Table ConversationT where
  data PrimaryKey ConversationT f = ConversationKey (Columnar f Int)
    deriving (Beamable, Generic)
  primaryKey = ConversationKey <$> conversationId
type Conversation = ConversationT Identity
type ConversationId = PrimaryKey ConversationT Identity

deriving instance Eq Conversation
deriving instance Show Conversation
