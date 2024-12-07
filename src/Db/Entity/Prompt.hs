module Db.Entity.Prompt where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam (Identity)
import Database.Beam.Schema (Beamable, Columnar, PrimaryKey(..), Table)
import GHC.Generics (Generic)

data PromptT f = Prompt { promptId        :: Columnar f Int32
                        , promptGuid      :: Columnar f UUID
                        , promptPrompt    :: Columnar f Text
                        , promptMemberId  :: Columnar f Text
                        , promptFriendId  :: Columnar f Text
                        , promptEnabled   :: Columnar f Bool
                        , promptCreatedAt :: Columnar f UTCTime
                        } deriving (Beamable, Generic)

instance Table PromptT where
  data PrimaryKey PromptT f = PromptKey (Columnar f Int32)
    deriving (Beamable, Generic)
  primaryKey = PromptKey <$> promptId
type Prompt = PromptT Identity
type PromptId = PrimaryKey PromptT Identity

deriving instance Eq Prompt
deriving instance Show Prompt
