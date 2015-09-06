{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module View.Profile where

import           Controller.Utils
import           Data.Text                        (Text)
import           Data.Time.Clock                  ()
import           Lucid
import           Model
import           Static
import           Text.Digestive
import           Text.Digestive.Lucid.Html5
import           Text.Html.Email.Validate
import           View.Register
import           View.Utils
import           Web.Apiary                       hiding (Html, string, text)
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession

profileForm :: Text -> Form Text (ActionT '[Session Text IO, Persist, Logger] prms IO) Profile
profileForm u = Profile
    <$> "oldPassword"  .: checkM ("Wrong password" :: Text) isOldPassword (text Nothing)
    <*> "newPassword"  .: (text Nothing)
    <*> "email" .: check ("Not a valid email address" :: Text) isValidEmail (text Nothing)
    <*> "desc" .: text Nothing
  where
    isOldPassword :: Text ->  (ActionT '[Session Text IO, Persist, Logger] prms IO Bool)
    isOldPassword p = verifyUser u p


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
        p_ "Welcome back, you can update you credentials:"
        with (profileView $ fmap toHtml lform) [id_ "loginForm"]
        p_ "Or add a new user:"
        with (registerView $ fmap toHtml rform) [id_ "registerForm"]
