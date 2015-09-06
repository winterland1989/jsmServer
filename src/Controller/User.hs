{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.User where

import           Control.Monad
import qualified Data.Aeson                          as JSON
import           Data.Char
import           Data.Int
import           Data.Proxy
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Data.Time.Clock
import           Database.Persist.Postgresql
import           Lucid
import           Model
import qualified Network.Wai.Parse                   as P
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
            contentType "text/html"
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
                    lazyBytes . renderBS $ profilePage lform' rform
                Nothing -> do
                    lform <- getForm "login" loginForm
                    lazyBytes . renderBS $ loginPage lform rform

        method POST . action $ do
            userParams <- getReqBodyParams
            u <- getSession pText
            let mkFormEnv = (\_-> return $ paramsToEnv userParams)
            let renderPage page f = do
                 contentType "text/html"
                 rform <- getForm "register" registerForm
                 lazyBytes . renderBS $ page f rform

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
                                Nothing -> renderPage profilePage pform
                        Nothing -> do
                            deleteSession pText
                            redirect "/"

                Nothing -> do
                    (lform, linfo) <- postForm "login" loginForm mkFormEnv
                    case linfo of
                        Just l  -> do
                            setSession pText $ loginName l
                            redirect "/"
                        Nothing -> renderPage loginPage lform

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
                contentType "text/html"
                lform <- getForm "login" loginForm
                lazyBytes . renderBS $ loginPage lform rform

    [capture|/logout|] . method GET . action $ deleteSession pText >> redirect "/"

    [capture|/user/user::Text|] . method GET . action $ do
        contentType "text/html"
        user <- param [key|user|]
        ss <- runSql $ selectList [SnippetAuthor ==. user] [Desc SnippetMtime]
        (runSql $ get (UserKey user)) >>= \case
            Just u' -> do
                u <- getSession pText
                lazyBytes . renderBS . userPage u u'
                    $ map (\(Entity _ snippet) -> snippet) ss
            _ -> notFound404Page
