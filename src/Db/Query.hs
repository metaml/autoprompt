module Db.Query where

import Data.Aeson (Value, toJSON)
import Data.Text (Text)
import Database.Beam ( (==.), (&&.)
                     , all_, currentTimestamp_, default_, desc_, filter_, limit_, orderBy_,  val_
                     , runInsert, insert, insertExpressions
                     , runSelectReturningList, select
                     )
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import Db.Entity.Conversation
import Db.Entity.Enum
import Db.Entity.Prompt
import Api.Model (Message(..))
import Prelude hiding (id)
import qualified Db.Db as Db

type MemberId = Text
type FriendId = Text

appConversation :: Connection -> Message -> IO ()
appConversation cnx msg = do
  let query = insert Db.amiDb.conversation
              $ insertExpressions [ Conversation default_
                                                 default_
                                                 (val_ msg.member)
                                                 (val_ msg.friend)
                                                 (val_ Ami)
                                                 (val_ $ if msg.role == "assistant" then Friend else Member)
                                                 (val_ msg.content)
                                                 (val_ $ toJSON msg)
                                                 (val_ Itm)
                                                 currentTimestamp_
                                  ]
  runBeamPostgres cnx $ runInsert query
  pure ()

history :: Connection -> MemberId -> FriendId -> IO [Value]
history cnx mid fid = history' cnx mid fid Nothing

history' :: Connection -> MemberId -> FriendId -> Maybe Integer -> IO [Value]
history' cnx mid fid rows = do
  let count = \r -> case r of
                      Just n  -> n
                      Nothing -> 31
      query = limit_ (count rows)
              $ orderBy_ (desc_ . conversationId)
              $ filter_ (\c -> c.conversationMemberId ==. val_ mid &&. c.conversationFriendId ==. val_ fid)
                        (all_ Db.amiDb.conversation)
  cs <- runBeamPostgres cnx
        $ runSelectReturningList
        $ select query
  pure $ (\c -> c.conversationMessage) <$> (reverse cs)

prompts :: Connection -> MemberId -> IO [Prompt]
prompts cnx mid = prompts' cnx mid (Just "system")

prompts' :: Connection -> MemberId -> Maybe FriendId -> IO [Prompt]
prompts' cnx mid fid = do
  let fid' = case fid of
               Just x  -> x
               Nothing -> "system"
      query = filter_ (\p -> p.promptMemberId ==. val_ mid
                             &&. p.promptFriendId ==. val_ fid'
                             &&. p.promptEnabled ==. val_ True
                      )
                      (all_ Db.amiDb.prompt)
  ps <- runBeamPostgres cnx
        $ runSelectReturningList
        $ select query
  pure ps
