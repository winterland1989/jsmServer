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
import           Data.ByteString
import           Data.Time.Clock
import           Database.Persist.TH

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
    revision Int
    mtime UTCTime
    download Int
    Foreign User fkAuthor author
    UniqueSnippet author title version
    deriving Show

SearchMap
    keyword Text
    snippet SnippetId
    UniqueSearchMap keyword snippet
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
        registerName      :: Text
    ,   registerPassword  :: Text
    ,   registerEmail     :: Text
    ,   registerDesc      :: Text
    } deriving (Show)

data LoginInfo = LoginInfo {
        loginName     :: Text
    ,   loginPassword :: Text
    } deriving (Show)

data Profile = Profile {
        oldPassword :: Text
    ,   newPassword :: Text
    ,   newEmail :: Text
    ,   newDesc :: Text
    } deriving (Show)
