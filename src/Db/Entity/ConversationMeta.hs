module Db.Entity.ConversationMeta where

import Data.Aeson (Value)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam (Identity)
import Database.Beam.Schema (Beamable, Columnar, PrimaryKey(..), Table)
import Db.Entity.Enum (MetaEnum)
import GHC.Generics (Generic)

data ConversationMetaT f = ConversationMeta { conversationMetaId                 :: Columnar f Int32
                                            , conversationMetaGuid               :: Columnar f UUID
                                            , conversationMetaMemberId           :: Columnar f Text
                                            , conversationMetaFriendId           :: Columnar f Text
                                            , conversationMetaLastConversationId :: Columnar f Int
                                            , conversationMetaMetaType           :: Columnar f MetaEnum
                                            , conversationMetaMetaData           :: Columnar f Value
                                            , conversationCreatedAt              :: Columnar f UTCTime
                                            } deriving (Beamable, Generic)

instance Table ConversationMetaT where
  data PrimaryKey ConversationMetaT f = ConversationKey (Columnar f Int32)
    deriving (Beamable, Generic)
  primaryKey = ConversationKey <$> conversationMetaId

type ConversationMeta = ConversationMetaT Identity
type ConversationMetaId = PrimaryKey ConversationMetaT Identity

deriving instance Eq ConversationMeta
deriving instance Show ConversationMeta
