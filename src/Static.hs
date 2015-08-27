{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Static where

import           Data.FileEmbed
import           Data.Text          (Text)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

indexScript :: Text
indexScript = T.decodeUtf8 $(embedFile "src/Static/index.js")

snippetScript :: Text
snippetScript = T.decodeUtf8 $(embedFile "src/Static/snippet.js")

loginScript :: Text
loginScript = T.decodeUtf8 $(embedFile "src/Static/login.js")
