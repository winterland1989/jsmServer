{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module View.Register where

import           Data.Maybe                       (isJust)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time.Clock                  ()
import           Database.Persist.Postgresql
import           Lucid
import           Model
import           Text.Digestive
import           Text.Digestive.Lucid.Html5
import           Text.Html.Email.Validate
import           Web.Apiary                       hiding (Html, string, text)
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession

registerForm :: Form Text (ActionT '[Session Text IO, Persist, Logger] prms IO) RegisterInfo
registerForm = RegisterInfo
    <$> "name" .: (check ("Name can't be empty" :: Text) (not . T.null) .
        check ("Name reserved" :: Text) isNotReserved .
        checkM ("Name already used" :: Text) isUnqUser) (text Nothing)
    <*> "password"  .: check ("Can't use empty password" :: Text) (not . T.null) (text Nothing)
    <*> "email" .: check ("Not a valid email address" :: Text) isValidEmail (text Nothing)
    <*> "desc" .: text Nothing
  where
    isUnqUser name = (runSql . get $ SUserKey name) >>= return . not . isJust
    isNotReserved name = not $ name `elem` ["jsm"]

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
