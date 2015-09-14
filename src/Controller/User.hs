{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Controller.User where

import           Control.Monad
import           Control.Monad.Apiary.Action
import           Controller.Utils
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as T
import           Database.Persist.Postgresql
import           Model
import           System.Entropy
import           Text.Digestive.Types
import           Text.Digestive.View
import           View.Login
import           View.Profile
import           View.Register
import           View.User
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession

userRouter :: (Has SessionExt exts, Has Persist exts) => ApiaryT exts '[] IO m ()
userRouter = do

    [capture|/login|] $ do
        method GET . action $ do
            rform <- getForm "register" registerForm
            u <- getSession'
            case u of
                Just u'  -> do
                    Just user <- runSql $ get (SUserKey u')
                    lform <- getForm "profile" (profileForm u')
                    let lform' = lform {viewInput = [
                            (toPath "email", TextInput $ sUserEmail user)
                        ,   (toPath "desc", TextInput $ sUserDesc user)
                        ]}
                    lucidRes $ profilePage lform' rform
                Nothing -> do
                    lform <- getForm "login" loginForm
                    lucidRes $ loginPage lform rform

        method POST . action $ do
            u <- getSession'
            case u of
                Just u'  -> do
                    (pform, profile) <- postForm "profile" (profileForm u') mkFormEnv
                    case profile of
                        Just p  -> do
                            liftIO $ print profile
                            let newP = newPassword p
                            salt <- liftIO $ getEntropy 32
                            when (newP /= "") (runSql $ updateWhere [ SUserName ==. u']
                                [ SUserPwdHash =. hashPassword newP salt , SUserSalt =. salt ])

                            runSql $ updateWhere [ SUserName ==. u']
                                [ SUserEmail =. newEmail p , SUserDesc =. newDesc p ]

                            redirect $ "/user/" <> T.encodeUtf8 u'
                        Nothing -> renderLoginPage profilePage pform

                Nothing -> do
                    (lform, linfo) <- postForm "login" loginForm mkFormEnv
                    case linfo of
                        Just l  -> do
                            setSession pText $ loginName l
                            redirect "/"
                        Nothing -> renderLoginPage loginPage lform

    [capture|/register|] . method POST . action $ do
        (rform, reg) <- postForm "register" registerForm mkFormEnv
        case reg of
            Just r -> do
                salt <- liftIO $ getEntropy 32
                _ <- runSql . insert $ SUser
                    (registerName r)
                    salt
                    (hashPassword (registerPassword r) salt)
                    (registerEmail r)
                    (registerDesc r)

                setSession pText $ registerName r
                redirect "/"
            Nothing -> do
                lform <- getForm "login" loginForm
                lucidRes $ loginPage lform rform

    [capture|/logout|] . method GET . action $ deleteSession pText >> redirect "/"

    [capture|/user/user::Text|] . method GET . action $ do
        user <- param [key|user|]
        ss <- runSql $ selectList [SnippetAuthor ==. user] [Desc SnippetMtime]
        (runSql $ get (SUserKey user)) >>= \case
            Just u' -> do
                u <- getSession'
                lucidRes $ userPage u u'
                    $ map entityVal ss
            _ -> notFound404Page

  where
    renderLoginPage page f = do
        rform <- getForm "register" registerForm
        lucidRes $ page f rform
