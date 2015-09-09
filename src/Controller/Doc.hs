{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.Doc where

import           Data.Text                           (Text)
import           Database.Persist.Postgresql
import           Model
import           View.Doc
import           Controller.Utils
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession
import           Static


documentRouter :: Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
documentRouter = [capture|/doc/docName::Text|] . method GET . action $ do
    u <- getSession'
    docName <- param [key|docName|]
    lucidRes $ docPage u (mkDoc docName)

  where
    mkDoc docName = case docName of
        "cn" -> cnDocHtml

        _ -> enDocHtml

