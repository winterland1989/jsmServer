{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase         #-}

module View.Login where

import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Lucid
import           Model
import           Static
import           Text.Digestive
import           Text.Digestive.Lucid.Html5
import           Web.Apiary                       hiding (Html, string, text)
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession

import           View.Register
import           View.Utils
import           Controller.Utils (verifyUser)

loginForm :: Form Text (ActionT '[Session Text IO, Persist, Logger] prms IO) LoginInfo
loginForm = checkM ("Wrong name/password" :: Text) loginCheck(
    LoginInfo
        <$> "name" .: check ("Name can't be empty" :: Text) (not . T.null) (text Nothing)
        <*> "password" .: (text Nothing)
    )
  where
    loginCheck (LoginInfo name password) = verifyUser name password

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
    pageTitle  "Login <|> Register | jsm"
    script_ loginScript
    body_ $ do
        p_ "Welcome back, please login with you credentials:"
        with (loginView $ fmap toHtml lform) [id_ "loginForm"]
        p_ "Or add a new user:"
        with (registerView $ fmap toHtml rform) [id_ "registerForm"]


