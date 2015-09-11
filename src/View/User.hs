{-# LANGUAGE OverloadedStrings #-}

module View.User where

import           Data.Text                           (Text)
import     qualified      Data.Text as T
import     qualified Data.Aeson as JSON
import           Lucid
import           Data.Monoid
import           Data.Time.Clock                  ()
import           Model
import           Static
import           CMark
import View.Utils
import Control.Monad
import qualified Data.Vector as V

userPage ::  SessionInfo -> SUser -> [Snippet] -> Html ()
userPage u u' ss = doctypehtml_ . html_ $ do
    pageTitle $ sUserName u' <> " | jsm"
    body_ $ do
        topBar u
        div_ [id_ "userDesc"] $ do
            h1_ $ do
                span_ "Email: "
                let email = sUserEmail u'
                a_ [href_ $ "mailto:" <> email] $ toHtml email
            (toHtmlRaw $ commonmarkToHtml [] (sUserDesc u'))

        div_ [id_ "userList"] $ do
            h1_ . toHtml $ sUserName u' <> "'s snippets:"
            ul_ $
                forM_ ss $ \(Snippet author title _ language version revision deprecated keywords _ _ mtime) ->
                    li_ $ do
                        div_ [class_ "CodeInfo"] $ do
                            a_ [href_ $  "/snippet/" <> author <> "/" <> title <> "/" <> textShow version] $
                                toHtml (title <> textShow version)
                            when deprecated $
                                span_ [class_ "Deprecated"] "DEPRECATED"
                            if revision == 0
                                then span_ . toHtml $ "uploaded@" <> textShow mtime
                                else span_ . toHtml $ "revised@" <> textShow mtime
                            span_ . toHtml $ language <> "," <> " revision" <> textShow revision

                        div_ [class_ "CodeInfo"] $ do
                            span_ "keywords:"
                            span_ . toHtml $ T.intercalate " " (keywordsToList keywords)

        script_ userPageScript

  where
    keywordsToList keywords =
        let (JSON.Array keywords') = (JSON.toJSON keywords)
        in V.toList $ V.map (\(JSON.String w) -> w) keywords'
