{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.Snippet where

import           Control.Monad
import           Control.Monad.Apiary.Action
import           Data.Char
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Time.Clock
import           Database.Persist.Postgresql
import           Lucid
import           Model
import           View.Snippet
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession
import Controller.Utils


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

                (author, pwdHash, title, version, language, content)
                    <- [params|author, pwdHash, title, version, language, content|]
                if validTile title == True
                    then do
                    (runSql . getBy $ UniqueUser author pwdHash) >>= \case
                        Just _ -> do
                            logging "post snippet"
                            now <- liftIO getCurrentTime

                            Entity sid snippet <- runSql $ upsert
                                (Snippet author title content language version (-1) now 0)
                                [   SnippetRevision +=. 1
                                ,   SnippetContent  =. content
                                ,   SnippetMtime    =. now
                                ,   SnippetLanguage =. language
                                ]

                            when (snippetRevision snippet == 0) $ do
                                let kw' = map (flip SearchMap $ sid) (extractKeyWord title)
                                runSql $ insertMany_ kw'

                            jsonRes snippet

                        _ -> notFound404Api
                    else notFound404Api

        method DELETE .
            ([key|author|] =: pText) .
            ([key|pwdHash|] =: pText) .
            ([key|title|] =: pText) . action $ do
                stop

  where
    validTile title = and $ isLetter <$> T.unpack title
    extractKeyWord = T.split isUpper
