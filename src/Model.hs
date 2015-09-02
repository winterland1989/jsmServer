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
import           Lens.Simple

share [mkPersist sqlSettings{ mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    pwdHash Text
    email Text
    desc Text
    PrimaryUserName name
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
    UniqueSearchMap keyword snippet
    deriving Show

Comment json
    snippet SnippetId
    user  Text
    content Text
    mtime UTCTime default=CURRENT_TIME
    deriving Show
|]

type SessionInfo = Maybe Text

data LoginInfo = LoginInfo {
        _loginName     :: Text
    ,   _loginPassword :: Text
    } deriving (Show)

data Profile = Profile {
        _oldPassword :: Text
    ,   _newPassword :: Text
    ,   _newEmail :: Text
    ,   _newDesc :: Text
    }

makeLenses ''LoginInfo
makeLenses ''Profile

