{-# LANGUAGE OverloadedStrings #-}
module View.Snippet where

import           Data.Text                           (Text)
import           Lucid
import           Data.Monoid
import           Data.Time.Clock                  ()
import           Model
import           Static
import View.Utils
import Database.Persist.Sql

snippetPage :: SessionInfo -> SnippetId -> Snippet -> Html ()
snippetPage u sid
    (Snippet author title content language version keywords revision download mtime) =
    doctypehtml_ . html_ $ do
        pageTitle $ title <> textShow revision
        script_ [src_ aceScriptCdnUrl] ("" :: Text)
        body_ $ do
            topBar u
            div_ [id_ "editor", data_ "language" language] $ toHtml content
            div_ [id_ "sideBar"] $ do
                div_ [id_ "snippetInfo"] $
                    mapM_  (p_ . toHtml) [
                            "Title:" <> title
                        ,   "Author: " <> author
                        ,   "Version: " <> textShow version
                        ,   "Revision: " <> textShow revision
                        ,   "Mtime: " <> textShow mtime
                        ,   "Download: " <> textShow download
                        ,   "Language: " <> language
                        ]
                let sid' = textShow $ fromSqlKey sid
                div_ [id_ "commentInput", data_ "sid" sid'] $
                    case u of
                        Just _ -> div_ [id_ "commentForm"] ""
                        Nothing -> a_ [href_ "/login"] "Login to discuss"
                div_ [id_ "comment"] ""

            script_ snippetScript
