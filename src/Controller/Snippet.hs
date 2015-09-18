{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Controller.Snippet where

import           Control.Monad.Apiary.Action
import           Control.Monad.Logger
import           Controller.Utils
import qualified Data.Aeson                       as JSON
import           Data.Apiary.Param
import           Data.Char
import qualified Data.HashMap.Strict              as Map
import           Data.Maybe
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time.Clock
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import           Database.Persist.Postgresql
import           Database.Persist.Postgresql.Json
import           Model
import           Network.Routing.Dict
import           Text.Digestive.Types
import           Text.Digestive.View
import           View.Snippet
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Text.InterpolatedString.Perl6

snippetWithForm :: (Has SessionExt exts, Has Persist exts,
    Member "author" Text prms, Member "title" Text prms, Member "version" Int prms)
    => Maybe (View Text)
    -> ActionT exts prms IO ()
snippetWithForm cform = do
    (author, title, version) <- [params|author, title, version|]
    (runSql . getBy) (UniqueSnippet author title version) >>= \case
        Just (Entity sid snippet) -> do
            comments <- runSql $ selectList [CommentSnippet ==. sid] [LimitTo 100, Desc CommentMtime]
            requires <- runSql $ selectList [SnippetURIRequires @>. JSON.toJSON (fromSqlKey sid)] [Desc SnippetURITitle]
            u <- getSession'
            let comments' = map entityVal comments
            let requires' = map entityVal requires
            case cform of
                Just cform' -> lucidRes $
                    snippetPage u cform' comments' requires' snippet
                Nothing -> do
                    newCform <- getForm "comment" commentForm
                    let newCform' = newCform {viewInput = [
                            (toPath "sid", TextInput $ textShow $ fromSqlKey sid)
                        ]}
                    lucidRes $ snippetPage u newCform' comments' requires' snippet
        _ -> notFound404Api

snippetRouter :: (Has SessionExt exts, Has Persist exts, Has Logger exts)
    => ApiaryT exts '[] IO m ()
snippetRouter = do
    [capture|/snippet/author::Text/title::Text/version::Int|] $ do

        method GET . action $ snippetWithForm Nothing

        method POST . action $
            getSession' >>= \case
                Just u -> do
                    (cform, comment) <- postForm "comment" commentForm mkFormEnv
                    now <- liftIO getCurrentTime
                    case comment of
                        Just (sid, content) -> do
                            runSql . insert_ $ Comment (toSqlKey sid) u content now
                            snippetWithForm Nothing
                        Nothing -> snippetWithForm $ Just cform

                Nothing -> redirect "/login"

    [capture|/snippet|] $ do

        method GET .
            ([key|author|] =: pText) .
            ([key|title|] =: pText) .
            ([key|version|] =: pInt) .
            document "Get snippet json with given author, title and version." . action $ do
                (author, title, version) <- [params|author, title, version|]
                logInfoN $ [qc|Get snippet {author}/{title}/{version}|]
                (runSql . getBy) (UniqueSnippet author title version) >>= \case
                    Just (Entity sid snippet) -> do
                        runSql $ update sid [SnippetDownload +=. 1]
                        jsonRes $ addIdToSnippetJson sid snippet
                    _ -> notFound404Api

        method POST .
            ([key|author|] =: pText) .
            ([key|password|] =: pText) .
            ([key|title|] =: pText) .
            ([key|version|] =: pInt) .
            ([key|keywords|] ?? "json array" =: pLazyByteString) .
            ([key|requires|] ?? "json array" =: pLazyByteString) .
            ([key|language|] =: pText) .
            ([key|content|] =: pText) .
            document "Publish snippet" . action $ do

                (author, password, title, version, keywords, requires, language, content)
                    <- [params|author, password, title, version, keywords, requires, language, content|]

                let keywords' = fmap (fmap T.toLower) (JSON.decode keywords) :: Maybe (Vector Text)

                let requires' = JSON.decode requires :: Maybe (Vector Int)

                if validTile title && isJust keywords' && isJust requires'
                    then verifyUser author password >>= \case
                        True -> do
                            logInfoN $ [qc|Publish snippet {author}/{title}/{version}|]
                            now <- liftIO getCurrentTime

                            let keywords'' = Jsonb $ JSON.toJSON $ fromJust keywords'
                            let requires'' = Jsonb $ JSON.toJSON $ fromJust requires'

                            Entity sid snippet <- runSql $ upsert
                                (Snippet author title content language version (-1) False
                                    keywords'' requires'' 0 now)
                                [   SnippetRevision +=. 1
                                ,   SnippetDeprecated =. False
                                ,   SnippetLanguage =. language
                                ,   SnippetContent  =. content
                                ,   SnippetKeywords =. keywords''
                                ,   SnippetRequires =. requires''
                                ,   SnippetMtime    =. now
                                ]

                            V.forM_ (fromJust keywords') $ \w -> runSql . insertUnique $ Keyword w

                            jsonRes $ addIdToSnippetJson sid snippet
                        False -> status networkAuthenticationRequired511
                    else status badRequest400
                stop

        method DELETE .
            ([key|author|] =: pText) .
            ([key|password|] =: pText) .
            ([key|title|] =: pText) .
            ([key|version|] =: pInt) .
            document "Deprecate snippet" . action $ do
                (author, password, title, version) <- [params|author, password, title, version|]
                verifyUser author password >>= \case
                    True -> do
                        c <- runSql $ count [
                                SnippetAuthor ==. author
                            ,   SnippetTitle ==. title
                            ,   SnippetVersion ==. version
                            ]
                        if c == 0
                            then status notFound404
                            else do
                                logInfoN $ [qc|Deprecate snippet {author}/{title}/{version}|]
                                runSql $ updateWhere [
                                        SnippetAuthor ==. author
                                    ,   SnippetTitle ==. title
                                    ,   SnippetVersion ==. version
                                    ]
                                    [ SnippetDeprecated =. True ]
                    False -> status networkAuthenticationRequired511
                stop

  where
    validTile = T.all isLetter

    addIdToSnippetJson sid snippet =
        let JSON.Object o = JSON.toJSON snippet
        in Map.insert "id" (JSON.toJSON sid) o
