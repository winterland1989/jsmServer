{-# LANGUAGE OverloadedStrings #-}

module View.Doc where

import           Data.Text       (Text)
import           Data.Time.Clock ()
import           Lucid
import           Model
import           Static
import           View.Utils

docPage :: SessionInfo -> Text -> Html ()
docPage u doc = doctypehtml_ . html_ $ do
    pageTitle "Document | jsm"

    body_ $ do
        topBar u
        div_ [class_ "Content"] $ toHtmlRaw doc

        script_ docScript

