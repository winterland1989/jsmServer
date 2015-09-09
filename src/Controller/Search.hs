{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.Search where

import           Controller.Utils
import           Control.Monad.Logger
import qualified Data.Aeson                       as JSON
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Database.Persist.Postgresql
import           Database.Persist.Postgresql.Json
import           Model
import           View.Search
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession


searchItemPerPage :: Int
searchItemPerPage = 10

searchRouter :: Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
searchRouter = method GET $ do

    [capture|/search|] .
        ([key|keywords|] =: pText) .
        ([key|sort|] =: pText) .
        ([key|page|] =?!: (0 :: Int)) . action $ do
            (keywords, sort, page) <- [params|keywords, sort, page|]
            slength <- runSql $ count
                [ SnippetKeywords @>. JSON.toJSON (T.words keywords) ]
            logInfoN $ textShow (T.words keywords)
            snippets <- runSql $ selectList
                [ SnippetKeywords @>. JSON.toJSON (T.words keywords) ]
                [   sortBy sort
                ,   OffsetBy $ searchItemPerPage * page
                ,   LimitTo $ searchItemPerPage * (page + 1)
                ]
            u <- getSession'
            lucidRes $ searchPage u slength (map entityVal snippets)

    [capture|/keywords|] . ([key|word|] =: pText) . action $ do
        word <- param [key|word|]
        kws <- runSql $ rawSql "SELECT word FROM keyword WHERE word LIKE ?%" []
        jsonRes $ map (keywordWord . entityVal) kws

  where
    sortBy sort = case sort of
        "mtime" -> Asc SnippetMtime
        "download" -> Asc SnippetDownload
        _ -> Asc SnippetTitle
