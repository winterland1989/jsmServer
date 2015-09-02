{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module View (
    indexPage
,   userPage
,   snippetPage
,   loginPage
,   profilePage
,   registerForm
,   profileForm
,   loginForm
,   notFoundPage
,   hash'
) where

import           Crypto.Hash
import           Control.Monad
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
import           Lens.Simple
import           CMark
import Data.Maybe (isJust)

textShow :: Show a => a -> Text
textShow = T.pack . show

hash' :: Text -> Text
hash' = T.decodeUtf8 . digestToHexByteString . md5 . T.encodeUtf8
  where
    md5 :: BS.ByteString -> Digest MD5
    md5 = hash

title' :: Text -> Html ()
title' t = head_ $ do
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
    div_ [id_ "userinfo"] $ do
        case u of
            Just username -> do
                a_ [class_ "WelcomeMsg", href_ $ "/user/" <> username] . toHtml $ "Welcome, " <> username
                span_ "|"
                a_ [class_ "ProfileBtn", href_ "/login"] "profile"
                span_ "|"
                a_ [class_ "LoginOutBtn", href_ "/logout"] "logout"
            Nothing ->
                a_ [class_ "LoginBtn", href_ "/login"] "login | register"

indexPage :: SessionInfo -> [Snippet] -> Html ()
indexPage u ss = doctypehtml_ . html_ $ do
    title' "Introduction | jsm"
    script_ [src_ aceScriptCdnUrl] ("" :: Text)
    body_ $ do
        topBar u
        div_ [id_ "introduction"] $ do
            h1_ "Welcome to jsm, a javascript snippet manager."
            p_ $ toHtmlRaw introMarkDown
        div_ [id_ "latestList"] $ do
            h1_ "Lastest published:"
            ul_ $
                forM_ ss $ \(Snippet author title content language version revision mtime _) -> do
                    li_ $ do
                        div_ [class_ "CodeInfo"] $ do
                            a_ [href_ $  "/user/" <> author] $ toHtml author
                            if revision == 0
                                then span_ "uploaded"
                                else span_ "revised"
                            a_ [href_ $  "/" <> author <> "/" <> title <> "/" <> textShow version] $ do
                                span_ $ toHtml (title <> textShow version)
                            span_ $ "@"
                            span_ . toHtml . show $ mtime

                        div_ [class_ "CodePreview", data_ "language" language] . toHtml $
                            (T.unlines . take 8 . T.lines $ content) <> "..."

        script_ indexScript

userPage ::  SessionInfo -> User -> [Snippet] -> Html ()
userPage u u' ss = doctypehtml_ . html_ $ do
    title' $ (u' ^. userName) <> " | jsm"
    script_ [src_ aceScriptCdnUrl] ("" :: Text)
    body_ $ do
        topBar u
        div_ [id_ "userDesc"] (toHtmlRaw $ commonmarkToHtml [] $ u' ^. userDesc)
        div_ [id_ "userList"] $ do
            h1_ .toHtml $ (u' ^. userName) <> "'s snippets:"
            ul_ $
                forM_ ss $ \(Snippet author title _ language version revision mtime _) -> do
                    li_ $ do
                        div_ [class_ "CodeInfo"] $ do
                            a_ [href_ $  "/" <> author <> "/" <> title <> "/" <> textShow version] $
                                toHtml (title <> textShow version)
                            span_ . toHtml $ language <> "," <> " revision" <> textShow revision
                            if revision == 0
                                then span_ "uploaded"
                                else span_ "revised"
                            span_ $ "@"
                            span_ . toHtml . show $ mtime

        script_ userPageScript


snippetPage :: SessionInfo -> SnippetId -> Snippet -> Html ()
snippetPage u sid (Snippet author title content language version revision mtime download) =
    doctypehtml_ . html_ $ do
        title' $ title <> textShow revision
        script_ [src_ aceScriptCdnUrl] ("" :: Text)
        body_ $ do
            topBar u
            div_ [id_ "editor", data_ "language" language] $ toHtml content
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
                        Just _ -> div_ [id_ "commentForm"] ""
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
    isUnqUser name = (runSql . getBy $ PrimaryUserName name) >>= return . not . isJust

registerView :: Monad m => View (HtmlT m ()) -> HtmlT m ()
registerView v = form v "/register" $ do
    label     "name" v "Name:"
    inputText "name" v
    errorList "name" v
    label     "password" v "Password:"
    inputText "password" v
    errorList "password" v
    label     "email" v "Email address:"
    inputText "email" v
    errorList "email" v
    label     "desc" v "Describe(can use markdown):"
    inputTextArea Nothing Nothing "desc" v
    errorList "desc" v
    with (inputSubmit "Register") [class_ "SubmitBtn"]

loginForm :: Form Text (ActionT '[Session Text IO, Persist, Logger] prms IO) LoginInfo
loginForm = checkM ("Wrong name/password" :: Text) loginCheck(
    LoginInfo
        <$> "name" .: check ("Name can't be empty" :: Text) (not . T.null) (text Nothing)
        <*> "password" .: (text Nothing)
    )
  where
    loginCheck (LoginInfo name password) =
        (runSql . getBy $ UniqueUser name (hash' password)) >>= return . isJust

loginView :: Monad m => View (HtmlT m ()) -> HtmlT m ()
loginView v = form v "/login" $ do
    label     "name" v "Name:"
    inputText "name" v
    errorList "name" v
    label     "password" v "Password:"
    inputText "password" v
    errorList "" v
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

profileForm :: Text -> Form Text (ActionT '[Session Text IO, Persist, Logger] prms IO) Profile
profileForm u = Profile
    <$> "oldPassword"  .: checkM ("Wrong password" :: Text) isOldPassword (text Nothing)
    <*> "newPassword"  .: (text Nothing)
    <*> "email" .: check ("Not a valid email address" :: Text) isValidEmail (text Nothing)
    <*> "desc" .: text Nothing
  where
    isOldPassword :: Text ->  (ActionT '[Session Text IO, Persist, Logger] prms IO Bool)
    isOldPassword p = do
        (runSql . getBy $ UniqueUser u (hash' p)) >>= return . isJust


profileView :: Monad m => View (HtmlT m ()) -> HtmlT m ()
profileView v = form v "/login" $ do
    label     "oldPassword" v "Old password:"
    inputText "oldPassword" v
    errorList "oldPassword" v
    label     "newPassword" v "New password, leave empty to keep old password:"
    inputText "newPassword" v
    errorList "newPassword" v
    label     "email" v "Email address:"
    inputText "email" v
    errorList "email" v
    label     "desc" v "Describe(can use markdown):"
    inputTextArea Nothing Nothing "desc" v
    errorList "desc" v
    with (inputSubmit "Update profile") [class_ "SubmitBtn"]

profilePage :: View Text -> View Text -> Html ()
profilePage lform rform = doctypehtml_ . html_ $ do
    title'  "Profile <|> Register | jsm"
    script_ loginScript
    body_ $ do
        p_ "Welcome back, you can update you credentials:"
        with (profileView $ fmap toHtml lform) [id_ "loginForm"]
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

