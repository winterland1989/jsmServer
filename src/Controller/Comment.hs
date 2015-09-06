{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.Comment where

import           Control.Monad.Apiary.Action
import           Data.Int
import           Data.Proxy
import           Data.Text                           (Text)
import           Data.Time.Clock
import           Database.Persist.Postgresql
import           Model
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession
import Controller.Utils

commentRouter ::  Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
commentRouter = do
    [capture|/comment|] $ do
        method POST . ([key|sid|] =: pInt64) . ([key|content|] =: pText) . action $ do
            (sid, content) <- [params|sid, content|]
            getSession pText >>= \case
                Just u -> do
                    now <- liftIO getCurrentTime
                    runSql . insert_ $ Comment (toSqlKey sid) u content now
                    stop
                Nothing -> redirect "/login"

        method GET . ([key|sid|] =: pInt64) .
            ([key|page|] =: pInt) .
            ([key|perPage|] =: pInt) .
            action $ do
            (sid, page, perPage) <- [params|sid, page, perPage|]
            cmts <- runSql $ selectList [CommentSnippet ==. toSqlKey sid]
                [Asc CommentMtime, LimitTo perPage, OffsetBy $ (page - 1) * perPage]
            jsonRes cmts
  where
    pInt64 :: Proxy Int64
    pInt64 = Proxy
