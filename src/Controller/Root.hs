{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.Root where

import           Data.Text                           (Text)
import           Database.Persist.Postgresql
import           Lucid
import           Model
import           View.Index
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession


rootRouter :: Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
rootRouter = root . method GET . action $ do
    contentType "text/html"
    ss <- runSql $ selectList [] [(LimitTo 20), Desc SnippetMtime]
    u <- getSession pText
    lazyBytes . renderBS . indexPage u
        $ map (\(Entity _ snippet) -> snippet) ss
