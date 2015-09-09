{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Static where

import           Data.FileEmbed
import           Data.Text          (Text)
import qualified Data.Text.Encoding as T
import           CMark

introHtml :: Text
introHtml = commonmarkToHtml [] $ T.decodeUtf8 $(embedFile "src/Static/intro.md")

enDocHtml :: Text
enDocHtml = commonmarkToHtml [] $ T.decodeUtf8 $(embedFile "src/Static/enDoc.md")

cnDocHtml :: Text
cnDocHtml = commonmarkToHtml [] $ T.decodeUtf8 $(embedFile "src/Static/cnDoc.md")

indexScript :: Text
indexScript = T.decodeUtf8 $(embedFile "src/Static/index.js")

docScript :: Text
docScript = T.decodeUtf8 $(embedFile "src/Static/doc.js")

searchScript :: Text
searchScript = T.decodeUtf8 $(embedFile "src/Static/search.js")

userPageScript :: Text
userPageScript = T.decodeUtf8 $(embedFile "src/Static/user.js")

snippetScript :: Text
snippetScript = T.decodeUtf8 $(embedFile "src/Static/snippet.js")

loginScript :: Text
loginScript = T.decodeUtf8 $(embedFile "src/Static/login.js")

aceScriptCdnUrl :: Text
aceScriptCdnUrl = "//cdn.staticfile.org/ace/1.1.3/ace.js"

normalizeCssCDNUrl :: Text
normalizeCssCDNUrl = "//cdn.staticfile.org/normalize/3.0.1/normalize.min.css"

githubCssText :: Text
githubCssText = T.decodeUtf8 $(embedFile "src/Static/markdown.css")

