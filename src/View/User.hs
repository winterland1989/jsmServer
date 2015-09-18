{-# LANGUAGE OverloadedStrings #-}

module View.User where

import           CMark
import           Data.Monoid
import           Data.Time.Clock ()
import           Lucid
import           Model
import           Static
import           View.Utils

userPage ::  SessionInfo -> SUser -> [Snippet] -> Html ()
userPage u u' ss = doctypehtml_ . html_ $ do
    pageTitle $ sUserName u' <> " | jsm"
    body_ $ do
        topBar u
        div_ [class_ "Content"] $ do
            h1_ $ do
                span_ "Email: "
                let email = sUserEmail u'
                a_ [href_ $ "mailto:" <> email] $ toHtml email
            toHtmlRaw $ commonmarkToHtml [] (sUserDesc u')

        div_ [class_ "Content"] $ do
            h1_ . toHtml $ sUserName u' <> "'s snippets:"
            ul_ $ mapM_ (li_ [class_ "Code"] . codePreview) ss

        script_ userPageScript
