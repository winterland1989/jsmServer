{-# LANGUAGE OverloadedStrings #-}

module View (
    indexPage
,   snippetPage
,   userView, userForm
,   notFoundPage
) where

import           Data.Foldable              hiding (for_)
import           Data.Monoid
import           Data.Text
import           Data.Time.Clock
import           Lucid
import           Model
import           Static
import           Text.Digestive
import           Text.Digestive.Lucid.Html5
import           Text.Html.Email.Validate

textShow :: Show a => a -> Text
textShow = pack . show

titleHead :: Text -> Html ()
titleHead t = head_ $ do
        meta_ [charset_ "UTF-8"]
        title_ $ toHtml t
        style_ normalizeStyle

topBar :: SessionInfo -> Html ()
topBar u = div_ [id_ "topBar"] $ do
    div_ [id_ "search"] ""
    div_ [id_ "userinfo"] $ do
        case u of
            Just username -> do
                a_ [class_ "WelcomeMsg", href_ $ "/" <> username] . toHtml $ "Welcome, " <> username
                a_ [class_ "LoginOutBtn", href_ "/user/logout"] "logout"
            Nothing ->
                a_ [class_ "LoginBtn", href_ "/login"] "login | register"

indexPage :: SessionInfo -> [Snippet] -> Html ()
indexPage u ss = doctypehtml_ . html_ $ do
    titleHead "Introduction | jsm"
    body_ $ do
        topBar u
        div_ [id_ "introduction"] $ do
            h1_ "Welcome to jsm, the javascript snippet manager."
            p_ "wip"
        ul_ [id_ "latestList"] $
            forM_ ss $ \(Snippet author title content language version revision mtime _) -> do
                li_ $ do
                    a_ [href_ $  "/" <> author] $ toHtml author
                    if revision == 0
                        then span_ "uploaded"
                        else span_ "revised"
                    a_ [href_ $  "/" <> title <> "/" <> textShow version] $ do
                        span_ $ toHtml title
                        span_ . toHtml . show $ version
                    span_ $ "@"
                    span_ . toHtml . show $ mtime
        script_ indexScript

snippetPage :: Snippet -> Html ()
snippetPage (Snippet author title content language version revision mtime download) =
    doctypehtml_ . html_ $ do
        titleHead title'
        body_ $ do
            div_ [id_ "source"] $ toHtml content
            div_ [id_ "comment"] $ do
                p_ $ toHtml title'
            script_ snippetScript
  where
    title' = title <> textShow version

userForm :: Monad m => Form Text m User
userForm = User
    <$> "name" .: text Nothing
    <*> "password"  .: text Nothing
    <*> "mail" .: check ("Not a valid email address" :: Text) isValidEmail (text Nothing)
    <*> "desc" .: text Nothing

userView :: Monad m => View (HtmlT m ()) -> HtmlT m ()
userView view = do
    label     "name" view "Name: "
    inputText "name" view

    errorList "mail" view
    label     "mail" view "Email address: "
    inputText "mail" view

{-
loginPage :: Html ()
loginPage = doctypehtml_ . html_ $ do
    titleHead  "Login <|> Register | jsm"
    body_ $ do
        form_ [id_  "loginForm"] $ do
            label_ [for_ "username"] "Name:"
            input_ [id_ "username"]
            label_ [for_ "password"] "Password:"
            input_ [id_ "password"]
            input_ [type_ "submit", value_ "Login"]
        userView
-}
notFoundPage :: Html ()
notFoundPage = doctypehtml_ . html_ $ do
    titleHead  "Not found | jsm"
    body_ $ do
        div_
            [style_ "width: 100%; height: 100%; line-height: 100%;\
                \font-size: 100px; text-align: center; margin-top: 200px;" ]
            "404 dude, Get out of here!!!"

