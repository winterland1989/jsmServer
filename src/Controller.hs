{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase   #-}

module Controller (
    webRouter
,   userApiRouter
,   snippetApiRouter
,   commentRouter
,   notFound404Router
) where

import           Control.Monad.Apiary.Action
import           Control.Monad.Apiary.Filter.Capture
import qualified Data.Aeson                          as JSON
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Data.Time.Clock
import           Database.Persist.Sqlite
import           Lucid
import           Model
import           View
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession
import           Crypto.Hash.SHA256 (hash)
import           Text.Digestive (getForm)

notFound404Page :: ActionT ext prms IO ()
notFound404Page = do
    status notFound404
    contentType "text/html"
    lazyBytes . renderBS $ notFoundPage

notFound404Api :: ActionT ext prms IO ()
notFound404Api = status notFound404 >> stop

notFound404Router :: Monad m => ApiaryT ext prms IO m ()
notFound404Router = anyPath $ action notFound404Page

webRouter :: Monad m => ApiaryT '[Session T.Text IO, Persist, Logger] '[] IO m ()
webRouter = method GET $ do

    root . action $ do
        contentType "text/html"
        ss <- runSql $ selectList [] [(LimitTo 20), Desc SnippetMtime]
        u <- getSession pText
        lazyBytes . renderBS . indexPage u
            $ map (\(Entity _ snippet) -> snippet) ss

    [capture|/login|] . action $ do
        contentType "text/html"
        --lazyBytes $ renderBS loginPage
        v <- getForm "test" userForm
        lazyBytes . renderBS . userView $ fmap toHtml  v

    [capture|/author::Text/title::Text/version::Int|] . action $ do
        (author, title, version) <- [params|author, title, version|]
        s <- runSql $ getBy (UniqueSnippet author title version)
        case s of
            Just (Entity _ snippet) -> do
                contentType "text/html"
                lazyBytes . renderBS $ snippetPage snippet
            _ -> notFound404Page


userApiRouter :: Monad m => ApiaryT '[Session T.Text IO, Persist, Logger] '[] IO m ()
userApiRouter = method POST $ do

    [capture|/user/check|] . action $
        getSession pText >>= \case
            Just name -> text name
            _ -> notFound404Api

    [capture|/user/login|] . userGuard . action $ do
        (name, password) <- [params|name, password|]
        (runSql . getBy $ UniqueUser name (hash' password)) >>= \case
            Just _ -> setSession pText name
            _ -> notFound404Api

    [capture|/user/logout|] . action $ deleteSession pText

    [capture|/user/register|] . userGuard .
        ([key|desc|]  =: pText) . ([key|email|] =: pText) .
        action $ do
        (name, password, email, desc) <- [params|name, password, email, desc|]
        u <- runSql $ insertUnique (User name (hash' password) email desc)
        case u of
            Just _ -> setSession pText name
            _ -> notFound404Api

  where
    userGuard = ([key|name|]  =: pText) . ([key|password|] =: pText)
    hash' = T.pack . show . hash . T.encodeUtf8

commentRouter ::  Monad m => ApiaryT '[Session T.Text IO, Persist, Logger] '[] IO m ()
commentRouter = do
    [capture|/comment|] $ do
        method POST .  ([key|snippetId|] =: pString) . ([key|content|] =: pText) . action $ do
            (snippetId, content) <- [params|snippetId, content|]
            getSession pText >>= \case
                Just name -> do
                    _ <- runSql $ insert (Comment (read snippetId) name content)
                    stop
                _ -> notFound404Page

        method GET . ([key|snippetId|] =: pString) . action $ do
            snippetId <- param [key|snippetId|]
            stop

snippetApiRouter :: Monad m => ApiaryT '[Session T.Text IO, Persist, Logger] '[] IO m ()
snippetApiRouter =
    [capture|/api/author::Text/title::Text/version::Int|] $ do

        method GET . action $ do
            (author, title, version) <- [params|author, title, version|]
            s <- runSql $ getBy (UniqueSnippet author title version)

            case s of
                Just (Entity sId snippet) -> do
                    runSql $ update sId [SnippetDownload +=. 1]
                    jsonRes snippet

                _ -> notFound404Api

        method POST . action $ do
            (author, title, version) <- [params|author, title, version|]
            p <- getReqBodyParams
            case (,) <$> (lookup "language" p) <*> (lookup "content" p) of
                Just (language, content) -> do
                    logging "post snippet"
                    let language' = T.decodeUtf8 language
                    let content' = T.decodeUtf8 content
                    now <- liftIO getCurrentTime

                    Entity _ snippet <- runSql $ upsert
                        (Snippet author title content' language' version (-1) now 0)
                        [   SnippetRevision +=. 1
                        ,   SnippetContent  =. content'
                        ,   SnippetMtime    =. now
                        ,   SnippetLanguage =. language'
                        ]

                    jsonRes snippet

                _ -> notFound404Api

  where
    jsonRes = lazyBytes . JSON.encode
