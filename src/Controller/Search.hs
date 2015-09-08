{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.Search where

import           Controller.Utils
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

    [capture|/search|] . ([key|keywords|] =: pText) . ([key|page|] =?!: (0 :: Int)) . action $ do
        (keywords, page) <- [params|keywords, page|]
        slength <- runSql $ count
            [ SnippetKeywords <@. JSON.toJSON (T.words keywords) ]
        snippets <- runSql $ selectList
            [ SnippetKeywords <@. JSON.toJSON (T.words keywords) ]
            [   Asc SnippetMtime
            ,   OffsetBy $ searchItemPerPage * page
            ,   LimitTo $ searchItemPerPage * (page + 1)
            ]
        u <- getSession'
        if null snippets
            then redirect "/"
            else lucidRes $ searchPage u slength (map entityVal snippets)

    [capture|/keywords|] . ([key|word|] =: pText) . action $ do
        word <- param [key|word|]
        kws <- runSql $ rawSql "SELECT word from keyword where word=?" []
        jsonRes $ map (keywordWord . entityVal) kws
