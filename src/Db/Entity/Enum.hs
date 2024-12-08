{-# language UndecidableInstances #-}
module Db.Entity.Enum where

import Data.Aeson (FromJSON, ToJSON)
import Data.Strings (strCapitalize, strToLower)
import Database.Beam.Backend.SQL (sqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.CustomTypes (FromBackendRow, HasSqlValueSyntax)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS

data FriendEnum  = Human | Ami
  deriving (Enum, Eq, Generic, Ord, Read, Show, ToJSON, FromJSON)

data MessageEnum = Itm | Stm | Ltm
  deriving (Enum, Eq, Generic, Ord, Read, Show, ToJSON, FromJSON)

data MetaEnum = Detail | Entity | Event | Sentiment | Summary | Theme
  deriving (Enum, Eq, Generic, Ord, Read, Show, ToJSON, FromJSON)

data SpeakerEnum = Member | Friend
  deriving (Enum, Eq, Generic, Ord, Read, Show, ToJSON, FromJSON)

--
instance HasSqlValueSyntax be String => HasSqlValueSyntax be FriendEnum where
  sqlValueSyntax = sqlValueSyntax . strToLower . show

-- monad fromBackendRow is dependent on, as in calls, fromField below
instance FromBackendRow Postgres FriendEnum

instance FromField FriendEnum where
  fromField _ en = case en of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: FriendEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"

--
instance HasSqlValueSyntax be String => HasSqlValueSyntax be MessageEnum where
  sqlValueSyntax = sqlValueSyntax . strToLower . show

instance FromBackendRow Postgres MessageEnum

instance FromField MessageEnum where
  fromField _ en = case en of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: MessageEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"

--

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MetaEnum where
  sqlValueSyntax = sqlValueSyntax . strToLower . show

instance FromBackendRow Postgres MetaEnum

instance FromField MetaEnum where
  fromField _ en = case en of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: MetaEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"
--
instance HasSqlValueSyntax be String => HasSqlValueSyntax be SpeakerEnum where
  sqlValueSyntax = sqlValueSyntax . strToLower . show

instance FromBackendRow Postgres SpeakerEnum

instance FromField SpeakerEnum where
  fromField _ en = case en of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: SpeakerEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"
