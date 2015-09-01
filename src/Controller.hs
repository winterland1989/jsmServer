{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Controller (
    rootRouter
,   userRouter
,   snippetRouter
,   commentRouter
,   notFound404Router
) where

import           Control.Monad.Apiary.Action
import           Control.Monad.Apiary.Filter.Capture
import           Crypto.Hash
import qualified Data.Aeson                          as JSON
import           Data.Char
import           Data.Int
import           Data.Proxy
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import qualified Data.ByteString as BS
import           Data.Time.Clock
import           Database.Persist.Sqlite
import           Lens.Simple
import           Lucid
import           Model
import qualified Network.Wai.Parse                   as P
import           Text.Digestive                      (getForm, postForm)
import           Text.Digestive.Types
import           View
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession

notFound404Page :: ActionT ext prms IO ()
notFound404Page = do
    status notFound404
    contentType "text/html"
    lazyBytes . renderBS $ notFoundPage

notFound404Api :: ActionT ext prms IO ()
notFound404Api = status notFound404 >> stop

notFound404Router :: Monad m => ApiaryT ext prms IO m ()
notFound404Router = anyPath $ action notFound404Page

rootRouter :: Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
rootRouter = root . method GET . action $ do
    contentType "text/html"
    ss <- runSql $ selectList [] [(LimitTo 20), Desc SnippetMtime]
    u <- getSession pText
    lazyBytes . renderBS . indexPage u
        $ map (\(Entity _ snippet) -> snippet) ss

searchRouter :: Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
searchRouter = [capture|/search|] . method GET . action $ do
    contentType "text/html"
    stop

jsonRes :: JSON.ToJSON a => a -> ActionT ext prms IO ()
jsonRes = lazyBytes . JSON.encode

paramsToEnv :: Monad m => [P.Param] -> Path -> m [FormInput]
paramsToEnv ((k, v):rest) p = do
    if  T.decodeUtf8 k == fromPath p
        then return [TextInput $ T.decodeUtf8 v]
        else paramsToEnv rest p
paramsToEnv _ _ = fail "Parameter not found"

userRouter :: Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
userRouter = do

    [capture|/login|] $ do
        method GET . action $ do
            contentType "text/html"
            lform <- getForm "login" loginForm
            rform <- getForm "register" registerForm
            lazyBytes . renderBS $ loginPage lform rform

        method POST . action $ do
            userParams <- getReqBodyParams
            (lform, linfo) <- postForm "login" loginForm (\_-> return $ paramsToEnv userParams)
            case linfo of
                Just i  -> do
                    setSession pText $ i ^. loginName
                    redirect "/"
                Nothing -> do
                    contentType "text/html"
                    rform <- getForm "register" registerForm
                    lazyBytes . renderBS $ loginPage lform rform

    [capture|/register|] . method POST . action $ do
        userParams <- getReqBodyParams
        (rform, user) <- postForm "register" registerForm (\_-> return $ paramsToEnv userParams)
        case user of
            Just u -> do
                _ <- runSql $ insert (over userPwdHash hash' u)
                setSession pText $ u ^. userName
                redirect "/"
            Nothing -> do
                contentType "text/html"
                lform <- getForm "login" loginForm
                lazyBytes . renderBS $ loginPage lform rform

    [capture|/logout|] . method GET . action $ deleteSession pText >> redirect "/"

    [capture|/home/user:pText|] .method GET . action $ do
        stop

  where
    md5 :: BS.ByteString -> Digest MD5
    md5 = hash
    hash' = T.decodeUtf8 . digestToHexByteString . md5 . T.encodeUtf8

commentRouter ::  Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
commentRouter = do
    [capture|/comment|] $ do
        method POST . ([key|sid|] =: pInt64) . ([key|content|] =: pText) . action $ do
            (sid, content) <- [params|sid, content|]
            getSession pText >>= \case
                Just u -> do
                    now <- liftIO getCurrentTime
                    runSql . insert_ $ Comment (toSqlKey sid) u content now
                    stop
                Nothing -> redirect "/login"

        method GET . ([key|sid|] =: pInt64) .
            ([key|page|] =: pInt) .
            ([key|perPage|] =: pInt) .
            action $ do
            (sid, page, perPage) <- [params|sid, page, perPage|]
            cmts <- runSql $ selectList [CommentSnippet ==. toSqlKey sid]
                [Asc CommentMtime, LimitTo perPage, OffsetBy $ (page - 1) * perPage]
            jsonRes cmts
  where
    pInt64 :: Proxy Int64
    pInt64 = Proxy

snippetRouter :: Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
snippetRouter = do

    [capture|/author::Text/title::Text/version::Int|] . method GET . action $ do
        (author, title, version) <- [params|author, title, version|]
        (runSql $ getBy (UniqueSnippet author title version)) >>= \case
            Just (Entity sid snippet) -> do
                u <- getSession pText
                contentType "text/html"
                lazyBytes . renderBS $ snippetPage u sid snippet
            _ -> notFound404Page

    [capture|/snippet|] $ do

        method GET .
            ([key|author|] =: pText) .
            ([key|title|] =: pText) .
            ([key|version|] =: pInt) . action $ do
                (author, title, version) <- [params|author, title, version|]
                (runSql $ getBy (UniqueSnippet author title version)) >>= \case
                    Just (Entity sId snippet) -> do
                        runSql $ update sId [SnippetDownload +=. 1]
                        jsonRes snippet
                    _ -> notFound404Api

        method POST .
            ([key|author|] =: pText) .
            ([key|pwdHash|] =: pText) .
            ([key|title|] =: pText) .
            ([key|version|] =: pInt) .
            ([key|language|] =: pText) .
            ([key|content|] =: pText) . action $ do
                (author, pwdHash, title, language, content)
                    <- [params|author, pwdHash, title, language, content|]
                liftIO $ print author
                liftIO $ print pwdHash
                (runSql . getBy $ UniqueUser author pwdHash) >>= \case
                    Just _ -> do
                        logging "post snippet"
                        now <- liftIO getCurrentTime

                        Entity sid snippet <- runSql $ upsert
                            (Snippet author title content language 0 (-1) now 0)
                            [   SnippetRevision +=. 1
                            ,   SnippetContent  =. content
                            ,   SnippetMtime    =. now
                            ,   SnippetLanguage =. language
                            ]

                        let kw' = map (flip SearchMap $ sid) (extractKeyWord title)
                        runSql $ insertMany_ kw'

                        jsonRes snippet

                    _ -> notFound404Api

        method DELETE .
            ([key|author|] =: pText) .
            ([key|pwdHash|] =: pText) .
            ([key|title|] =: pText) . action $ do
                stop

  where
    extractKeyWord = T.split isUpper
