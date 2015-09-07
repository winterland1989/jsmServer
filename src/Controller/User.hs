{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.User where

import           Control.Monad
import           Data.Text                           (Text)
import qualified Data.Text.Encoding                  as T
import           Database.Persist.Postgresql
import           Model
import           System.Entropy
import           Text.Digestive.Types
import           Text.Digestive.View
import           View.Login
import           View.Register
import           View.Profile
import           View.User
import           Web.Apiary
import           Control.Monad.Apiary.Action
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession
import Controller.Utils
import Data.Monoid

userRouter :: Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
userRouter = do

    [capture|/login|] $ do
        method GET . action $ do
            rform <- getForm "register" registerForm
            u <- getSession pText
            case u of
                Just u'  -> do
                    Just user <- runSql $ get (UserKey u')
                    lform <- getForm "profile" (profileForm u')
                    let lform' = lform {viewInput = [
                            (toPath "email", TextInput $ userEmail user)
                        ,   (toPath "desc", TextInput $ userDesc user)
                        ]}
                    lucidRes $ profilePage lform' rform
                Nothing -> do
                    lform <- getForm "login" loginForm
                    lucidRes $ loginPage lform rform

        method POST . action $ do
            userParams <- getReqBodyParams
            u <- getSession pText
            let mkFormEnv = (\_-> return $ paramsToEnv userParams)

            case u of
                Just u'  -> do
                    (runSql . get $ UserKey u') >>= \case
                        Just (User _ salt _ _ _) -> do
                            (pform, profile) <- postForm "profile" (profileForm u') mkFormEnv
                            case profile of
                                Just p  -> do
                                    let newP = newPassword p
                                    when (newP /= "") (runSql $ updateWhere
                                            [ UserName ==. u'] [ UserPwdHash =. hashPassword newP salt]
                                        )
                                    runSql $ updateWhere [ UserName ==. u'] [
                                            UserEmail =. newEmail p
                                        ,   UserDesc =. newDesc p
                                        ]
                                    redirect $ "/user/" <> T.encodeUtf8 u'
                                Nothing -> renderLoginPage profilePage pform
                        Nothing -> do
                            deleteSession pText
                            redirect "/"

                Nothing -> do
                    (lform, linfo) <- postForm "login" loginForm mkFormEnv
                    case linfo of
                        Just l  -> do
                            setSession pText $ loginName l
                            redirect "/"
                        Nothing -> renderLoginPage loginPage lform

    [capture|/register|] . method POST . action $ do
        userParams <- getReqBodyParams
        (rform, reg) <- postForm "register" registerForm (\_-> return $ paramsToEnv userParams)
        case reg of
            Just r -> do
                salt <- liftIO $ getEntropy 64
                _ <- runSql . insert $ User
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
        (runSql $ get (UserKey user)) >>= \case
            Just u' -> do
                u <- getSession pText
                lucidRes $ userPage u u'
                    $ map (\(Entity _ snippet) -> snippet) ss
            _ -> notFound404Page

  where
    renderLoginPage page f = do
        rform <- getForm "register" registerForm
        lucidRes $ page f rform
