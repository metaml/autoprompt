module Db.ConversationMeta where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam (Identity)
import Database.Beam.Schema (Beamable, Columnar, PrimaryKey(..), Table)
import Db.Enum (MetaEnum)
import GHC.Generics (Generic)

data ConversationMetaT f = ConversationMeta { conversationMetaId                 :: Columnar f Int
                                            , conversationMetaGuid               :: Columnar f UUID
                                            , conversationMetaMemberId           :: Columnar f Text
                                            , conversationMetaFriendId           :: Columnar f Text
                                            , conversationMetaLastConversationId :: Columnar f Int
                                            , conversationMetaMetaType           :: Columnar f MetaEnum
                                            , conversationMetaMetaData           :: Columnar f Value
                                            , conversationCreatedAt              :: Columnar f UTCTime
                                            } deriving (Beamable, Generic)

instance Table ConversationMetaT where
  data PrimaryKey ConversationMetaT f = ConversationKey (Columnar f Int)
    deriving (Beamable, Generic)
  primaryKey = ConversationKey <$> conversationMetaId

type ConversationMeta = ConversationMetaT Identity
type ConversationMetaId = PrimaryKey ConversationMetaT Identity

deriving instance Eq ConversationMeta
deriving instance Show ConversationMeta
