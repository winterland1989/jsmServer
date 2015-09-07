{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.Search where

import           Control.Monad.Apiary.Action
import           Control.Monad.Apiary.Filter.Capture
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
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession
import Controller.Utils


searchItemPerPage :: Int
searchItemPerPage = 10

searchRouter :: Monad m => ApiaryT '[Session Text IO, Persist, Logger] '[] IO m ()
searchRouter = method GET $ do

    [capture|/search|] .  ([key|keywords|] =: pText) .  ([key|page|] =: pInt) . action $ do
        stop

    [capture|/keywords|] .  ([key|keywords|] =: pText) .  ([key|page|] =: pInt) . action $ do
        contentType "text/html"
        stop
