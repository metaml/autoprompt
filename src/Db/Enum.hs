module Db.Enum where

import GHC.Generics

data FriendEnum  = Human | Ami
  deriving (Eq, Generic, Show)

data MessageEnum = Itm | Stm | Ltm
  deriving (Eq, Generic, Show)

data MetaEnum = Detail | Entity | Event | Sentiment | Summary | Theme
  deriving (Eq, Generic, Show)

data SpeakerEnum = Member | Friend
  deriving (Eq, Generic, Show)
