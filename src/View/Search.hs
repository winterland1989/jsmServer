{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module View.Search where

import           Control.Monad
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time.Clock               ()
import           Lucid
import           Model
import           Static
import           Text.InterpolatedString.Perl6
import           View.Utils

searchPage :: SessionInfo -> Int -> [Snippet] -> Html ()
searchPage u itermPerPage ss = doctypehtml_ . html_ $ do
    pageTitle "Introduction | jsm"
    body_ $ do
        topBar u
        div_ [id_ "searchDesc"] $ h1_ "Search for:"
        div_ [id_ "searchList"] $
            ul_ $
                forM_ (zip ss [1..itermPerPage]) $ \(Snippet
                    author title content language version revision deprecated keywords _  _ mtime, _) ->
                    li_ $ do
                        div_ [class_ "CodeInfo"] $ do
                            a_ [href_ $  "/user/" <> author] $ toHtml author
                            if revision == 0
                                then span_ "uploaded"
                                else span_ "revised"
                            a_ [ href_ [qc|/snippet/{author}/{title}/{textShow version}|] ] $
                                span_ $ toHtml (title <> textShow version)
                            span_ $ toHtml ([qc|(revision{textShow revision})|] :: Text)
                            span_ "@"
                            span_ . toHtml . show $ mtime
                            when deprecated $ span_ [class_ "snippetDeprecated"] "DEPRECATED"

                        div_ [class_ "CodePreview", data_ "language" language] . toHtml $
                            (T.unlines . take 8 . T.lines $ content) <> "..."

        script_ searchScript

