module Db.Entity.Conversation where

import Data.Aeson (Value)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime, LocalTime)
import Data.UUID (UUID)
import Database.Beam (Identity)
import Database.Beam.Schema (Beamable, Columnar, PrimaryKey(..), Table)
import Db.Entity.Enum
import GHC.Generics (Generic)

data ConversationT f = Conversation { conversationId           :: Columnar f Int32
                                    , conversationGuid         :: Columnar f UUID
                                    , conversationMemberId     :: Columnar f Text
                                    , conversationFriendId     :: Columnar f Text
                                    , conversationFriendType   :: Columnar f FriendEnum
                                    , conversationSpeakerType  :: Columnar f SpeakerEnum
                                    , conversationLine         :: Columnar f Text
                                    , conversationMessage      :: Columnar f Value
                                    , conversationMessageState :: Columnar f MessageEnum
                                    , conversationCreatedAt    :: Columnar f LocalTime
                                    } deriving (Beamable, Generic)

instance Table ConversationT where
  data PrimaryKey ConversationT f = ConversationKey (Columnar f Int32)
    deriving (Beamable, Generic)
  primaryKey = ConversationKey <$> conversationId
type Conversation = ConversationT Identity
type ConversationId = PrimaryKey ConversationT Identity

deriving instance Eq Conversation
deriving instance Show Conversation
