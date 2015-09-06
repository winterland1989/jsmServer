{-# LANGUAGE OverloadedStrings #-}

module View.Index where

import           Control.Monad   (forM_)
import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time.Clock ()
import           Lucid
import           Model
import           Static
import           View.Utils

indexPage :: SessionInfo -> [Snippet] -> Html ()
indexPage u ss = doctypehtml_ . html_ $ do
    pageTitle "Introduction | jsm"
    script_ [src_ aceScriptCdnUrl] ("" :: Text)
    body_ $ do
        topBar u
        div_ [id_ "introduction"] $ do
            h1_ "Welcome to jsm, a javascript snippet manager."
            p_ $ toHtmlRaw introHtml
        div_ [id_ "latestList"] $ do
            h1_ "Lastest published:"
            ul_ $
                forM_ ss $ \(Snippet author title content language version _ revision mtime _) ->
                    li_ $ do
                        div_ [class_ "CodeInfo"] $ do
                            a_ [href_ $  "/user/" <> author] $ toHtml author
                            if revision == 0
                                then span_ "uploaded"
                                else span_ "revised"
                            a_ [href_ $  "/" <> author <> "/" <> title <> "/" <> textShow version] $
                                span_ $ toHtml (title <> textShow version)
                            span_ "@"
                            span_ . toHtml . show $ mtime

                        div_ [class_ "CodePreview", data_ "language" language] . toHtml $
                            (T.unlines . take 8 . T.lines $ content) <> "..."

        script_ indexScript
