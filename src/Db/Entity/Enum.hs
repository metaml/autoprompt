{-# language UndecidableInstances #-}
module Db.Entity.Enum where

import Data.Strings (strCapitalize)
import Database.Beam.Backend.SQL (autoSqlValueSyntax, sqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.CustomTypes (FromBackendRow, HasSqlValueSyntax)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS

data FriendEnum  = Human | Ami
  deriving (Enum, Eq, Generic, Ord, Read, Show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be FriendEnum where
  sqlValueSyntax = autoSqlValueSyntax

-- monad fromBackendRow is dependent on, as in calls, fromField below
instance FromBackendRow Postgres FriendEnum

instance FromField FriendEnum where
  fromField _ en = case en of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: FriendEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"

--
data MessageEnum = Itm | Stm | Ltm
  deriving (Enum, Eq, Generic, Ord, Read, Show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MessageEnum where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres MessageEnum

instance FromField MessageEnum where
  fromField _ en = case en of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: MessageEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"

--
data MetaEnum = Detail | Entity | Event | Sentiment | Summary | Theme
  deriving (Enum, Eq, Generic, Ord, Read, Show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MetaEnum where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres MetaEnum

instance FromField MetaEnum where
  fromField _ en = case en of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: MetaEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"
--
data SpeakerEnum = Member | Friend
  deriving (Enum, Eq, Generic, Ord, Read, Show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be SpeakerEnum where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres SpeakerEnum

instance FromField SpeakerEnum where
  fromField _ en = case en of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: SpeakerEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"
