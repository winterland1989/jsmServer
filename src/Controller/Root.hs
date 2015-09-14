{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts #-}

module Controller.Root where

import           Data.Text                           (Text)
import           Database.Persist.Postgresql
import           Model
import           View.Index
import           Controller.Utils
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession

rootRouter :: (Monad m, Has SessionExt exts, Has Persist exts)
    => ApiaryT exts '[] IO m ()
rootRouter = root . method GET . action $ do
    ss <- runSql $ selectList [] [ LimitTo 20, Desc SnippetMtime ]
    u <- getSession'
    lucidRes $ indexPage u (map entityVal ss)
