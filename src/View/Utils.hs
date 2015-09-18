{-# LANGUAGE OverloadedStrings #-}

module View.Utils where

import qualified Data.Aeson                       as JSON
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time.Clock                  ()
import qualified Data.Vector                      as V
import           Database.Persist.Postgresql.Json
import           Lucid
import           Model
import           Static
import  Control.Monad

textShow :: Show a => a -> Text
textShow = T.pack . show

keywordsToList :: Jsonb -> [Text]
keywordsToList keywords =
    let JSON.Array keywords' = JSON.toJSON keywords
    in V.toList $ V.map (\(JSON.String w) -> w) keywords'

pageTitle :: Text -> Html ()
pageTitle t = head_ $ do
        meta_ [charset_ "UTF-8"]
        title_ $ toHtml t
        link_ [ href_ marxCssCDNUrl , rel_ "stylesheet" , type_ "text/css" ]
        script_ [src_ aceScriptCdnUrl] ("" :: Text)
        script_ "ace.config.set('basePath', '//cdnjs.cloudflare.com/ajax/libs/ace/1.2.0')"
        style_ baseCss

topBar :: SessionInfo -> Html ()
topBar u = div_ [id_ "topBar"] $ do
    a_ [id_ "indexLink", href_ "/"] "Jsm, a javascript snippet manager."
    div_ [id_ "search"] ""
    div_ [id_ "userinfo"] $
        case u of
            Just username -> do
                a_ [class_ "WelcomeMsg", href_ $ "/user/" <> username] . toHtml $ "Welcome, " <> username
                span_ "|"
                a_ [class_ "ProfileBtn", href_ "/login"] "profile"
                span_ "|"
                a_ [class_ "LoginOutBtn", href_ "/logout"] "logout"
            Nothing ->
                a_ [class_ "LoginBtn", href_ "/login"] "login | register"


codePreview :: Snippet -> Html ()
codePreview
    (Snippet author title content language version revision deprecated keywords _  _ mtime) = do
        div_ [class_ "CodeInfo"] $ do
            a_ [href_ $  "/user/" <> author] $ toHtml author
            if deprecated
                then span_ "deprecate"
                else if revision == 0
                    then span_ "uploaded"
                    else span_ "revised"
            a_ [href_ $  "/snippet/" <> author <> "/" <> title <> "/" <> textShow version] $
                span_ $ toHtml (title <> textShow version)
            span_ . toHtml $ "(revision" <> textShow revision <> ")"
            span_ "@"
            span_ . toHtml . show $ mtime
            when deprecated $ span_ [class_ "snippetDeprecated"] "DEPRECATED"
            span_ [class_ "CodeInfoKeywords"] $ do
                span_ "keywords:"
                forM_ (keywordsToList keywords) $ \word -> span_ (toHtml word)

        div_ [class_ "CodePreview", data_ "language" language] . toHtml $
            (T.unlines . take 8 . T.lines $ content) <> "..."
