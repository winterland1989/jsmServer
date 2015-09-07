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

import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)
import           Database.Persist.TH
import Database.Persist.Postgresql.Json (Jsonb)


share [mkPersist sqlSettings{ mpsGeneric = False },
    mkMigrate "migrateAll",
    mkDeleteCascade sqlSettings { mpsGeneric = False }] [persistLowerCase|
User
    name Text
    salt ByteString
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
    keywords Jsonb
    revision Int
    download Int
    mtime UTCTime
    Foreign User fkAuthor author
    UniqueSnippet author title version
    deriving Show

Keyword
    word Text
    Primary word
    deriving Show

RequireMap
    snippet SnippetId
    require SnippetId
    UniqueRequireMap snippet require
    deriving Show

Comment json
    snippet SnippetId
    user  Text
    content Text
    mtime UTCTime
    deriving Show
|]

type SessionInfo = Maybe Text

data RegisterInfo = RegisterInfo {
        registerName     :: Text
    ,   registerPassword :: Text
    ,   registerEmail    :: Text
    ,   registerDesc     :: Text
    } deriving (Show)

data LoginInfo = LoginInfo {
        loginName     :: Text
    ,   loginPassword :: Text
    } deriving (Show)

data Profile = Profile {
        oldPassword :: Text
    ,   newPassword :: Text
    ,   newEmail    :: Text
    ,   newDesc     :: Text
    } deriving (Show)
