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

documentRouter :: Has SessionExt exts => ApiaryT exts '[] IO m ()
documentRouter =
    [capture|/doc/docName::Text|] . method GET . action $ do
        docName <- param [key|docName|]
        u <- getSession'
        case docName of
            "cn" -> lucidRes $ docPage u cnDocHtml

            "en" -> lucidRes $ docPage u enDocHtml

            "api" ->
                defaultDocumentationAction def
                    {   documentTitle       = "Jsm API documentation"
                    ,   documentUseCDN      = False
                    }

            _ -> notFound404Page
