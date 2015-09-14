{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Controller.Doc where

import           Controller.Utils
import           Data.Text                        (Text)
import           Model
import           Static
import           View.Doc
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession

documentRouter :: Has SessionExt exts => ApiaryT exts '[] IO m ()
documentRouter = [capture|/doc/docName::Text|] . method GET . action $ do
    u <- getSession'
    docName <- param [key|docName|]
    lucidRes $ docPage u (mkDoc docName)

  where
    mkDoc docName = case docName of
        "cn" -> cnDocHtml

        _ -> enDocHtml

