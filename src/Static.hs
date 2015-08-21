{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Static where

import           Data.FileEmbed
import           Data.Text
import           Data.Text.Encoding

indexScript :: Text
indexScript = decodeUtf8 $(embedFile "src/Static/index.js")

snippetScript :: Text
snippetScript = decodeUtf8 $(embedFile "src/Static/snippet.js")

normalizeStyle :: Text
normalizeStyle = decodeUtf8 $(embedFile "src/Static/normalize.css")
