{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module View (
    indexPage
,   snippetPage
,   loginPage
,   registerForm
,   loginForm
,   notFoundPage
) where

import           Crypto.Hash
import           Data.Foldable                    (forM_)
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.ByteString as BS
import           Data.Time.Clock                  ()
import           Database.Persist.Sqlite
import           Lucid
import           Model
import           Static
import           Text.Digestive
import           Text.Digestive.Lucid.Html5
import           Text.Html.Email.Validate
import           Web.Apiary                       hiding (Html, string, text)
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession

textShow :: Show a => a -> Text
textShow = T.pack . show

title' :: Text -> Html ()
title' t = head_ $ do
        meta_ [charset_ "UTF-8"]
        title_ $ toHtml t
        link_
            [   href_ "//cdn.jsdelivr.net/normalize/3.0.3/normalize.min.css"
            ,   rel_ "stylesheet"
            ,   type_ "text/css"
            ]

topBar :: SessionInfo -> Html ()
topBar u = div_ [id_ "topBar"] $ do
    a_ [id_ "indexLink", href_ "/"] "Jsm, a javascript snippet manager."
    div_ [id_ "search"] ""
    div_ [id_ "userinfo"] $ do
        case u of
            Just username -> do
                a_ [class_ "WelcomeMsg", href_ $ "/" <> username] . toHtml $ "Welcome, " <> username
                a_ [class_ "LoginOutBtn", href_ "/logout"] "| logout"
            Nothing ->
                a_ [class_ "LoginBtn", href_ "/login"] "login | register"

indexPage :: SessionInfo -> [Snippet] -> Html ()
indexPage u ss = doctypehtml_ . html_ $ do
    title' "Introduction | jsm"
    body_ $ do
        topBar u
        div_ [id_ "introduction"] $ do
            h1_ "Welcome to jsm, a javascript snippet manager."
            p_ "wip"
        ul_ [id_ "latestList"] $
            forM_ ss $ \(Snippet author title content language version revision mtime _) -> do
                li_ $ do
                    a_ [href_ $  "/" <> author] $ toHtml author
                    if revision == 0
                        then span_ "uploaded"
                        else span_ "revised"
                    a_ [href_ $  "/" <> author <> "/" <> title <> "/" <> textShow version] $ do
                        span_ $ toHtml title
                        span_ . toHtml . textShow $ version
                    span_ $ "@"
                    span_ . toHtml . show $ mtime
        script_ indexScript


snippetPage :: SessionInfo -> SnippetId -> Snippet -> Html ()
snippetPage u sid (Snippet author title content language version revision mtime download) =
    doctypehtml_ . html_ $ do
        title' $ title <> textShow revision
        script_ [src_ "//cdn.jsdelivr.net/ace/1.2.0/min/ace.js"] ("" :: Text)
        body_ $ do
            topBar u
            div_ [id_ "editor"] $ toHtml content
            div_ [id_ "sideBar"] $ do
                div_ [id_ "snippetInfo"] $ do
                    mapM_  (p_ . toHtml) [
                            "Title:" <> title
                        ,   "Author: " <> author
                        ,   "Version: " <> textShow version
                        ,   "Revision: " <> textShow revision
                        ,   "Mtime: " <> textShow mtime
                        ,   "Download: " <> textShow download
                        ,   "Language: " <> language
                        ]
                let sid' = textShow $ fromSqlKey sid
                div_ [id_ "commentInput", data_ "sid" sid'] $
                    case u of
                        Just u -> div_ [id_ "commentForm"] ""
                        Nothing -> a_ [href_ "/login"] "Login to discuss"
                div_ [id_ "comment"] ""

            script_ snippetScript

registerForm :: Form Text (ActionT '[Session Text IO, Persist, Logger] prms IO) User
registerForm = User
    <$> "name" .: (check ("Name can't be empty" :: Text) (not . T.null) .
        checkM ("Name already used" :: Text) isUnqUser) (text Nothing)
    <*> "password"  .: check ("Can't use empty password" :: Text) (not . T.null) (text Nothing)
    <*> "email" .: check ("Not a valid email address" :: Text) isValidEmail (text Nothing)
    <*> "desc" .: text Nothing
  where
    isUnqUser name = (runSql . getBy $ PrimaryUserName name) >>= \case
        Just _ -> return False
        _ -> return True

registerView :: Monad m => View (HtmlT m ()) -> HtmlT m ()
registerView view = form view "/register" $ do
    label     "name" view "Name: "
    inputText "name" view
    errorList "name" view
    label     "password" view "Password"
    inputText "password" view
    errorList "password" view
    label     "email" view "Email address: "
    inputText "email" view
    errorList "email" view
    label     "desc" view "Describe(can use markdown):"
    inputTextArea Nothing Nothing "desc" view
    errorList "desc" view
    with (inputSubmit "Register") [class_ "SubmitBtn"]

loginForm :: Form Text (ActionT '[Session Text IO, Persist, Logger] prms IO) LoginInfo
loginForm = checkM ("Wrong name/password" :: Text) loginCheck(
    LoginInfo
        <$> "name" .: check ("Name can't be empty" :: Text) (not . T.null) (text Nothing)
        <*> "password" .: (text Nothing)
    )
  where
    md5 :: BS.ByteString -> Digest MD5
    md5 = hash
    hash' = T.decodeUtf8 . digestToHexByteString . md5 . T.encodeUtf8
    loginCheck (LoginInfo name password) =
        (runSql . getBy $ UniqueUser name (hash' password)) >>= \case
            Just _ -> return True
            _ -> return False

loginView :: Monad m => View (HtmlT m ()) -> HtmlT m ()
loginView view = form view "/login" $ do
    label     "name" view "Name: "
    inputText "name" view
    errorList "name" view
    label     "password" view "Password"
    inputText "password" view
    errorList "" view
    with (inputSubmit "Login") [class_ "SubmitBtn"]

loginPage :: View Text -> View Text -> Html ()
loginPage lform rform = doctypehtml_ . html_ $ do
    title'  "Login <|> Register | jsm"
    script_ loginScript
    body_ $ do
        p_ "Welcome back, please login with you credentials:"
        with (loginView $ fmap toHtml lform) [id_ "loginForm"]
        p_ "Or add a new user:"
        with (registerView $ fmap toHtml rform) [id_ "registerForm"]

notFoundPage :: Html ()
notFoundPage = doctypehtml_ . html_ $ do
    title'  "Not found | jsm"
    body_ $ do
        div_
            [style_ "width: 100%; height: 100%; line-height: 100%;\
                \font-size: 100px; text-align: center; margin-top: 200px;" ]
            "404 dude, Get out of here!!!"

