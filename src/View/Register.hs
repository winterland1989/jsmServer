{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module View.Register where

import           Control.Monad
import           Data.Char
import           Data.Maybe                  (isNothing)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock             ()
import           Database.Persist.Postgresql
import           Lucid
import           Model
import           Text.Digestive
import           Text.Digestive.Lucid.Html5
import           Text.Html.Email.Validate
import           Web.Apiary                  hiding (Html, string, text)
import           Web.Apiary.Database.Persist

registerForm :: (Has SessionExt exts, Has Persist exts)
    => Form Text (ActionT exts prms IO) RegisterInfo
registerForm = RegisterInfo
    <$> "name" .: (check ("Name can't be empty" :: Text) (not . T.null) .
        check "Sorry, but the name is reserved" isNotReserved .
        check "Name can only contain [a-zA-Z]" isValidName .
        checkM "Name already used" isUnqUser) (text Nothing)
    <*> "password"  .: check "Can't use empty password" (not . T.null) (text Nothing)
    <*> "email" .: check "Not a valid email address" isValidEmail (text Nothing)
    <*> "desc" .: text Nothing
  where
    isUnqUser name = liftM isNothing (runSql . get $ SUserKey name)
    isNotReserved name = name `notElem` ["jsm", "test"]
    isValidName = T.all isLetter

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
