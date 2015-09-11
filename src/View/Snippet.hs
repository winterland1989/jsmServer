{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module View.Snippet where

import           Data.Monoid
import           Data.Text                        (Text)
import           Data.Int                        (Int64)
import qualified Data.Text                        as T
import           Data.Time.Clock                  ()
import           Database.Persist.Sql
import           Lucid
import           Model
import           Static
import           Text.Digestive
import           Text.Digestive.Lucid.Html5
import           View.Utils
import           Web.Apiary                       hiding (Html, string, text)
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession
import Control.Monad

snippetPage :: SessionInfo -> View Text -> [Comment] -> Snippet -> Html ()
snippetPage u cform comments
    (Snippet author title content language version revision deprecated keywords requires download mtime) =
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
                div_ [id_ "snippetInfoComment"] $ do
                    case u of
                        Just _ ->  with (commentView $ fmap toHtml cform) [id_ "commentForm"]

                        Nothing -> a_ [href_ "/login"] "Login to discuss"

                    ul_ [id_ "commentList"] $
                        forM_  comments $ \(Comment _ author content mtime) ->
                            li_ $ toHtml content

            script_ snippetScript

commentForm :: Form Text (ActionT '[Session Text IO, Persist, Logger] prms IO) (Text ,Text)
commentForm = ((,))
    <$> "sid" .: (text Nothing)
    <*> "comment" .: check "Comment can't be empty" (not . T.null) (text Nothing)

commentView :: Monad m => View (HtmlT m ()) -> HtmlT m ()
commentView v = form v "" $ do
    label     "comment" v "Comment:"
    errorList "comment" v
    inputTextArea Nothing Nothing "comment" v
    inputHidden "sid" v
    with (inputSubmit "Comment") [class_ "SubmitBtn"]

