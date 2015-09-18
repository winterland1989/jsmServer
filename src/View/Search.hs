{-# LANGUAGE OverloadedStrings #-}

module View.Search where

import           Data.Time.Clock               ()
import           Lucid
import           Model
import           Static
import           View.Utils

searchPage :: SessionInfo -> Int -> [Snippet] -> Html ()
searchPage u itermPerPage ss = doctypehtml_ . html_ $ do
    pageTitle "Introduction | jsm"
    body_ $ do
        topBar u
        div_ [class_ "Content"] $ h1_ "Search for:"
        div_ [class_ "Content"] $
            ul_ $ mapM_ (li_ [class_ "Code"] . codePreview) (take itermPerPage ss)

        script_ searchScript

