module Db.Query where

import Data.Aeson (Value)
import Data.Text (Text)
import Database.Beam ( (==.), (&&.)
                     , all_, filter_, limit_, runSelectReturningList, select, val_
                     )
import Database.Beam.Postgres (runBeamPostgres, runBeamPostgresDebug)
import Database.PostgreSQL.Simple (Connection)
import Db.Entity.Conversation
import Db.Entity.Prompt
import Prelude hiding (id)
import qualified Db.Db as Db

history :: Connection -> Text -> Text -> Maybe Integer -> IO [Value]
history con member friend rows = do
  let rows' = case rows of
                Just n  -> n
                Nothing -> 31
      query = limit_ rows'
              $ filter_ (\c -> c.conversationMemberId ==. val_ member &&. c.conversationFriendId ==. val_ friend)
                        (all_ Db.amiDb.conversation)
  cs <- runBeamPostgresDebug putStrLn con
        $ runSelectReturningList
        $ select query
  pure $ (\c -> c.conversationMessage) <$> cs

prompts :: Connection -> Text -> Maybe Text -> IO [Prompt]
prompts con member friend  = do
  let friend' = case friend of
                  Just x  -> x
                  Nothing -> "system"
      query = filter_ (\p -> p.promptMemberId ==. val_ member &&. p.promptFriendId ==. val_ friend')
                      (all_ Db.amiDb.prompt)
  ps <- runBeamPostgresDebug putStrLn con
        $ runSelectReturningList
        $ select query
  pure ps
