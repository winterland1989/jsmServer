{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Static where

import           CMark
import           Data.FileEmbed
import           Data.Text          (Text)
import qualified Data.Text.Encoding as T

introHtml :: Text
introHtml = commonmarkToHtml [] $ T.decodeUtf8 $(embedFile "src/Static/markdown/intro.md")

enDocHtml :: Text
enDocHtml = commonmarkToHtml [] $ T.decodeUtf8 $(embedFile "src/Static/markdown/enDoc.md")

cnDocHtml :: Text
cnDocHtml = commonmarkToHtml [] $ T.decodeUtf8 $(embedFile "src/Static/markdown/cnDoc.md")

indexScript :: Text
indexScript = T.decodeUtf8 $(embedFile "src/Static/dist/index.js")

docScript :: Text
docScript = T.decodeUtf8 $(embedFile "src/Static/dist/doc.js")

searchScript :: Text
searchScript = T.decodeUtf8 $(embedFile "src/Static/dist/search.js")

userPageScript :: Text
userPageScript = T.decodeUtf8 $(embedFile "src/Static/dist/user.js")

snippetScript :: Text
snippetScript = T.decodeUtf8 $(embedFile "src/Static/dist/snippet.js")

loginScript :: Text
loginScript = T.decodeUtf8 $(embedFile "src/Static/dist/login.js")

aceScriptCdnUrl :: Text
aceScriptCdnUrl = "http://cs0.meituan.net/cf/ace/1.2.0/ace.js"

coffeeScriptCdnUrl :: Text
coffeeScriptCdnUrl = "//coffeescript.org/extras/coffee-script.js"

liveScriptCdnUrl :: Text
liveScriptCdnUrl = "http://cs0.meituan.net/cf/livescript/1.4.0/livescript-min.js"

marxCssCDNUrl :: Text
marxCssCDNUrl = "http://cs0.meituan.net/cf/marx/1.3.0/marx.min.css"

githubCssText :: Text
githubCssText = T.decodeUtf8 $(embedFile "src/Static/css/markdown.css")

