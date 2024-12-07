module Db.Entity.Member where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam (Identity)
import Database.Beam.Schema (Beamable, Columnar, PrimaryKey(..), Table)
import GHC.Generics (Generic)

data MemberT f = Member { memberId        :: Columnar f Int32
                        , memberEmail     :: Columnar f Text
                        , memberPassword  :: Columnar f Text
                        , memberFirstNam  :: Columnar f Text
                        , memberLastName  :: Columnar f Text
                        , memberDisables  :: Columnar f Bool
                        , memberCreatedAt :: Columnar f UTCTime
                        } deriving (Beamable, Generic)

instance Table MemberT where
  data PrimaryKey MemberT f = MemberKey (Columnar f Int32)
    deriving (Beamable, Generic)
  primaryKey = MemberKey <$> memberId
type Member = MemberT Identity
type MemberId = PrimaryKey MemberT Identity

deriving instance Eq Member
deriving instance Show Member
