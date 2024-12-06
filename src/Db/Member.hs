module Db.Member where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam (Identity)
import Database.Beam.Schema (Beamable, Columnar, PrimaryKey(..), Table)
import Db.Enum (MessageEnum)
import GHC.Generics (Generic)

data MemberT f = Member { memberId        :: Columnar f Int
                        , memberEmail     :: Columnar f Text
                        , memberPassword  :: Columnar f Text
                        , memberFirstNam  :: Columnar f Text
                        , memberLastName  :: Columnar f Text
                        , memberDisables  :: Columnar f Bool
                        , memberCreatedAt :: Columnar f UTCTime
                        } deriving (Beamable, Generic)

instance Table MemberT where
  data PrimaryKey MemberT f = MemberKey (Columnar f Int)
    deriving (Beamable, Generic)
  primaryKey = MemberKey <$> memberId
type Member = MemberT Identity
type MemberId = PrimaryKey MemberT Identity

deriving instance Eq Member
deriving instance Show Member
