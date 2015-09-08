{-# LANGUAGE OverloadedStrings #-}

module View.Utils where

import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Lucid
import           Data.Monoid
import           Data.Time.Clock                  ()
import           Model
import           Static

type SessionInfo = Maybe Text

textShow :: Show a => a -> Text
textShow = T.pack . show

pageTitle :: Text -> Html ()
pageTitle t = head_ $ do
        meta_ [charset_ "UTF-8"]
        title_ $ toHtml t
        link_
            [   href_ normalizeCssCDNUrl
            ,   rel_ "stylesheet"
            ,   type_ "text/css"
            ]

topBar :: SessionInfo -> Html ()
topBar u = div_ [id_ "topBar"] $ do
    a_ [id_ "indexLink", href_ "/"] "Jsm, a javascript snippet manager."
    div_ [id_ "search"] ""
    div_ [id_ "userinfo"] $
        case u of
            Just username -> do
                a_ [class_ "WelcomeMsg", href_ $ "/user/" <> username] . toHtml $ "Welcome, " <> username
                span_ "|"
                a_ [class_ "ProfileBtn", href_ "/login"] "profile"
                span_ "|"
                a_ [class_ "LoginOutBtn", href_ "/logout"] "logout"
            Nothing ->
                a_ [class_ "LoginBtn", href_ "/login"] "login | register"

