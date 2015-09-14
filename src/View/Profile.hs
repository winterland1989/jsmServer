{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module View.Profile where

import           Controller.Utils
import           Data.Text                   (Text)
import           Data.Time.Clock             ()
import           Lucid
import           Model
import           Static
import           Text.Digestive
import           Text.Digestive.Lucid.Html5
import           Text.Html.Email.Validate
import           View.Register
import           View.Utils
import           Web.Apiary                  hiding (Html, string, text)
import           Web.Apiary.Database.Persist

profileForm :: (Has SessionExt exts, Has Persist exts)
    => Text -> Form Text (ActionT exts prms IO) Profile
profileForm u = Profile
    <$> "oldPassword"  .: checkM ("Wrong password" :: Text) (verifyUser u) (text Nothing)
    <*> "newPassword"  .: text Nothing
    <*> "email" .: check ("Not a valid email address" :: Text) isValidEmail (text Nothing)
    <*> "desc" .: text Nothing

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
    pageTitle  "Profile <|> Register | jsm"
    script_ loginScript
    body_ $ do
        h1_ "Update you credentials"
        with (profileView $ fmap toHtml lform) [id_ "loginForm"]
        h1_ "Or add a new user"
        with (registerView $ fmap toHtml rform) [id_ "registerForm"]
