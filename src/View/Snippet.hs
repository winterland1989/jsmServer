{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module View.Snippet where

import           Control.Monad
import           Data.Int
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time.Clock               ()
import           Lucid
import           Model
import           Static
import           Text.Digestive
import           Text.Digestive.Lucid.Html5
import           Text.InterpolatedString.Perl6
import           View.Utils
import           Web.Apiary                    hiding (Html, string, text)

snippetPage :: SessionInfo -> View Text -> [Comment] -> [SnippetURI] -> Snippet -> Html ()
snippetPage u cform comments requires
    (Snippet author title content language version revision deprecated keywords _ download mtime) =
    doctypehtml_ . html_ $ do
        meta_ [charset_ "UTF-8"]
        pageTitle $ title <> textShow revision
        script_ [src_ liveScriptCdnUrl] ("" :: Text)
        script_ "window.LiveScript = require('LiveScript');"
        script_ [src_ coffeeScriptCdnUrl] ("" :: Text)
        body_ $ do
            topBar u
            div_ [id_ "editor", data_ "language" language] $ toHtml content
            div_ [id_ "sideBar"] $ do
                div_ [id_ "snippetInfo"] $ do
                    when deprecated $
                        p_ [class_ "snippetDeprecated"] "DEPRECATED"
                    p_ $ span_ "Author: " >> a_ [href_ [qc|/user/{author}|]] (toHtml author)
                    mapM_  (p_ . toHtml) [
                            "Title:" <> title
                        ,   "Version: " <> textShow version
                        ,   "Revision: " <> textShow revision
                        ,   "keywords: " <> T.intercalate " " (keywordsToList keywords)
                        ,   "Download: " <> textShow download
                        ,   "Mtime: " <> textShow mtime
                        ,   "Language: " <> language
                        ]
                    p_ "Required by: "
                    ul_ [id_ "snippetRequires"] $
                        forM_ requires $ \(SnippetURI author title version deprecated _) ->
                            li_ $ do
                                a_ [href_ $ [qc|/snippet/{author}/{title}/{textShow version}|] ]
                                    (toHtml ([qc|{author}/{title}{textShow version}|] :: Text))
                                when deprecated $ span_ [class_ "SnippetDeprecated"] "DEPRECATED"

                div_ [id_ "snippetComment"] $ do
                    case u of
                        Just _ ->  with (commentView $ fmap toHtml cform) [id_ "commentForm"]
                        Nothing -> a_ [href_ "/login"] "Login to discuss"

                    ul_ [id_ "commentList"] $
                        forM_  comments $ \(Comment _ author content mtime) ->
                            li_ $ do
                                p_ [class_ "CommentInfo"] $ do
                                    a_ [href_ $ "/user/" <> author] $ toHtml author
                                    span_ . toHtml $ "@" <> textShow mtime
                                p_ [class_ "CommentContent"] . toHtml $ content

            script_ snippetScript

commentForm :: Has SessionExt exts
    => Form Text (ActionT exts prms IO) (Int64 ,Text)
commentForm = (,)
    <$> "sid" .: stringRead "Internal Error" Nothing
    <*> "comment" .: check "Comment can't be empty" (not . T.null) (text Nothing)

commentView :: Monad m => View (HtmlT m ()) -> HtmlT m ()
commentView v = form v "" $ do
    label     "comment" v "Comment:"
    errorList "comment" v
    inputTextArea Nothing Nothing "comment" v
    inputHidden "sid" v
    with (inputSubmit "Comment") [class_ "SubmitBtn"]

