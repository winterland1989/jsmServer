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
import qualified Data.Vector as V
import           Data.Time.Clock
import           Database.Persist.Postgresql
import           Database.Persist.Postgresql.Json
import           Model
import           View.Snippet
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession
import Controller.Utils
import Data.Maybe
import qualified Data.Aeson as JSON

snippetRouter :: Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
snippetRouter = do

    [capture|/snippet/author::Text/title::Text/version::Int|] . method GET . action $ do
        (author, title, version) <- [params|author, title, version|]
        (runSql $ getBy (UniqueSnippet author title version)) >>= \case
            Just (Entity sid snippet) -> do
                u <- getSession pText
                lucidRes $ snippetPage u sid snippet
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
            ([key|password|] =: pText) .
            ([key|title|] =: pText) .
            ([key|version|] =: pInt) .
            ([key|keywords|] =: pText) .
            ([key|requires|] =: pText) .
            ([key|language|] =: pText) .
            ([key|content|] =: pText) . action $ do

                (author, password, title, version, keywords, requires, language, content)
                    <- [params|author, password, title, version, keywords, requires, language, content|]

                case validTile title of
                    True -> verifyUser author password >>= \case
                        True -> do
                            logging "post snippet"
                            now <- liftIO getCurrentTime

                            let keywords' = fromMaybe [] (decodeJsonText keywords)
                            let keywords'' = Jsonb $ JSON.toJSON keywords'

                            Entity sid _ <- runSql $ upsert
                                (Snippet
                                    author title content language version
                                    keywords'' (-1) 0 now)
                                [   SnippetRevision +=. 1
                                ,   SnippetContent  =. content
                                ,   SnippetKeywords  =. keywords''
                                ,   SnippetMtime    =. now
                                ,   SnippetLanguage =. language
                                ]

                            forM_ keywords' $ \w -> runSql . insertBy $ Keyword w

                            let requires' = fromMaybe [] $ decodeJsonText requires
                            forM_ requires' $ \req -> runSql . insertBy $ RequireMap sid req

                            status ok200
                        False -> status networkAuthenticationRequired511
                    False -> status badRequest400
                stop

        method DELETE .
            ([key|author|] =: pText) .
            ([key|pwdHash|] =: pText) .
            ([key|title|] =: pText) . action $ do
                stop

  where
    validTile title = and $ isLetter <$> T.unpack title
