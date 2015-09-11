{-# LANGUAGE OverloadedStrings #-}

module View.Doc where

import           Control.Monad   (forM_)
import           Data.Monoid
import           Data.Text       (Text)
import           Data.Time.Clock ()
import           Lucid
import           Static
import           View.Utils

docPage :: SessionInfo -> Text -> Html ()
docPage u doc = doctypehtml_ . html_ $ do
    pageTitle "Document | jsm"

    body_ $ do
        topBar u
        div_ [id_ "document"] $ toHtmlRaw doc

        script_ docScript

