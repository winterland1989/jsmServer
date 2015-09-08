{-# LANGUAGE OverloadedStrings #-}

module View.User where

import           Data.Text                           (Text)
import           Lucid
import           Data.Monoid
import           Data.Time.Clock                  ()
import           Model
import           Static
import           CMark
import View.Utils
import Control.Monad (forM_)

userPage ::  SessionInfo -> SUser -> [Snippet] -> Html ()
userPage u u' ss = doctypehtml_ . html_ $ do
    pageTitle $ sUserName u' <> " | jsm"
    script_ [src_ aceScriptCdnUrl] ("" :: Text)
    body_ $ do
        topBar u
        div_ [id_ "userDesc"] (toHtmlRaw $ commonmarkToHtml [] (sUserDesc u'))
        div_ [id_ "userList"] $ do
            h1_ .toHtml $ sUserName u' <> "'s snippets:"
            ul_ $
                forM_ ss $ \(Snippet author title _ language version _ revision mtime _) ->
                    li_ $
                        div_ [class_ "CodeInfo"] $ do
                            a_ [href_ $  "/snippet/" <> author <> "/" <> title <> "/" <> textShow version] $
                                toHtml (title <> textShow version)
                            span_ . toHtml $ language <> "," <> " revision" <> textShow revision
                            if revision == 0
                                then span_ "uploaded"
                                else span_ "revised"
                            span_ "@"
                            span_ . toHtml . show $ mtime

        script_ userPageScript
