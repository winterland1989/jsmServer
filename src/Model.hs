{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Model where

import           Data.Text
import           Data.Time.Clock
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    pwdHash Text
    email Text
    desc Text
    Primary name
    UniqueUser name pwdHash
    deriving Show

Snippet json
    author Text
    title Text
    content Text
    language Text
    version Int
    revision Int
    mtime UTCTime default=CURRENT_TIME
    download Int
    UniqueSnippet author title version
    deriving Show

SearchMap
    keyword Text
    snippet SnippetId
    deriving Show

Comment json
    snippet SnippetId
    user  Text
    content Text
    deriving Show
|]

type SessionInfo = Maybe Text

data LoginInfo = LoginInfo {
        loginName     :: Text
    ,   loginPassword :: Text
    } deriving (Show)
