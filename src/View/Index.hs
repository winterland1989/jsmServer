{-# LANGUAGE OverloadedStrings #-}

module View.Index where

import           Data.Time.Clock ()
import           Lucid
import           Model
import           Static
import           View.Utils

indexPage :: SessionInfo -> [Snippet] -> Html ()
indexPage u ss = doctypehtml_ . html_ $ do
    pageTitle "Introduction | jsm"
    body_ $ do
        topBar u
        div_ [class_ "Content"] $ do
            h1_ "Welcome to jsm, a javascript snippet manager."
            p_ $ toHtmlRaw introHtml
        div_ [class_ "Content"] $ do
            h1_ "Lastest published:"
            ul_ $ mapM_ (li_ [class_ "Code"] . codePreview) ss

        script_ indexScript
