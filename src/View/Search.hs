{-# LANGUAGE OverloadedStrings #-}

module View.Search where

import           Control.Monad   (forM_)
import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time.Clock ()
import           Lucid
import           Model
import           Static
import           View.Utils

searchPage :: SessionInfo -> Int -> [Snippet] -> Html ()
searchPage u len ss = doctypehtml_ . html_ $ do
    pageTitle "Introduction | jsm"
    script_ [src_ aceScriptCdnUrl] ("" :: Text)
    body_ $ do
        topBar u
        div_ [id_ "searchDesc"] $ do
            h1_ "Search for:"
        div_ [id_ "searchList"] $ do
            ul_ $
                forM_ ss $ \(Snippet
                    author title content language version revision deprecated keywords _  _ mtime) ->
                    li_ $ do
                        div_ [class_ "CodeInfo"] $ do
                            a_ [href_ $  "/user/" <> author] $ toHtml author
                            if revision == 0
                                then span_ "uploaded"
                                else span_ "revised"
                            a_ [href_ $  "/snippet/" <> author <> "/" <> title <> "/" <> textShow version] $
                                span_ $ toHtml (title <> textShow version)
                            span_ . toHtml $ "(revision" <> textShow revision <> ")"
                            span_ "@"
                            span_ . toHtml . show $ mtime

                        div_ [class_ "CodePreview", data_ "language" language] . toHtml $
                            (T.unlines . take 8 . T.lines $ content) <> "..."

        script_ searchScript

