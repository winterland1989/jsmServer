{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module View.Login where

import           Controller.Utils            (verifyUser)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Lucid
import           Model
import           Static
import           Text.Digestive
import           Text.Digestive.Lucid.Html5
import           View.Register
import           View.Utils
import           Web.Apiary                  hiding (Html, string, text)
import           Web.Apiary.Database.Persist

loginForm :: (Has SessionExt exts, Has Persist exts)
    => Form Text (ActionT exts prms IO) LoginInfo
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
        h1_ "Login with you credentials"
        with (loginView $ fmap toHtml lform) [id_ "loginForm"]
        h1_ "Or add a new user"
        with (registerView $ fmap toHtml rform) [id_ "registerForm"]


