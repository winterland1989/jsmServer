{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts #-}

module Controller.Search where

import           Controller.Utils
import           Control.Monad.Logger
import qualified Data.Aeson                       as JSON
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Database.Persist.Postgresql
import           Database.Persist.Postgresql.Json
import    qualified       Database.Esqueleto as E
import           Model
import           View.Search
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger


searchItemPerPage :: Int
searchItemPerPage = 100

completeLimit :: Int
completeLimit = 20

searchRouter :: (Has SessionExt exts, Has Persist exts, Has Logger exts)
    => ApiaryT exts '[] IO m ()
searchRouter = method GET $ do

    [capture|/search|] .
        ([key|keywords|] =: pText) .
        ([key|sort|] =?!: ( "mtime" :: Text)) .
        ([key|page|] =?!: (0 :: Int)) . action $ do
            (keywords, sort, page) <- [params|keywords, sort, page|]
            let keywords' = T.toLower keywords
            logInfoN $ textShow (T.words keywords)
            snippets <- runSql $ selectList
                [ SnippetKeywords @>. JSON.toJSON (T.words keywords') ]
                [   sortBy sort
                ,   OffsetBy $ searchItemPerPage * page
                ,   LimitTo $ searchItemPerPage * (page + 1) + 1
                ]
            u <- getSession'
            lucidRes $ searchPage u searchItemPerPage (map entityVal snippets)

    [capture|/keywords|] . ([key|predict|] =: pText) .
        document "Get list of keywords with a given predict." . action $ do
        predict <- param [key|predict|]
        keywords <- runSql $ E.select $
             E.from $ \kw -> do
                 E.where_ (kw E.^. KeywordWord `E.ilike` (E.val predict E.++. (E.%)))
                 E.orderBy [E.asc $ kw E.^. KeywordWord]
                 return (kw E.^. KeywordWord)
        jsonRes $ map E.unValue keywords

  where
    sortBy sort = case sort of
        "mtime"    -> Desc SnippetMtime
        "download" -> Desc SnippetDownload
        _          -> Asc SnippetTitle
