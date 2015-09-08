{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Controller.Utils where

import           Control.Monad.Apiary.Action
import           Control.Monad.Apiary.Filter.Capture
import           Crypto.Hash
import           Data.Aeson                          (FromJSON, ToJSON)
import qualified Data.Aeson                          as JSON
import           Data.ByteString                     (ByteString)
import           Data.ByteString.Lazy                (fromStrict)
import           Data.Monoid
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Database.Persist.Postgresql
import           Lucid
import           Model
import qualified Network.Wai.Parse                   as P
import           Text.Digestive.Types
import           View.NotFound
import           View.Utils                          (SessionInfo)
import           Web.Apiary                          hiding (Html)
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession

-- Notfound page, api...
notFound404Page :: ActionT ext prms IO ()
notFound404Page = do
    status notFound404
    contentType "text/html"
    lazyBytes . renderBS $ notFoundPage

notFound404Api :: ActionT ext prms IO ()
notFound404Api = status notFound404 >> stop

notFound404Router :: Monad m => ApiaryT ext prms IO m ()
notFound404Router = anyPath $ action notFound404Page

-- Http respond helper
jsonRes :: JSON.ToJSON a => a -> ActionT ext prms IO ()
jsonRes = lazyBytes . JSON.encode

textHtmlRes :: Text -> ActionT ext prms IO ()
textHtmlRes htmlText = contentType "text/html" >> text htmlText

lucidRes :: Html () -> ActionT ext prms IO ()
lucidRes l = contentType "text/html" >> (lazyBytes $ renderBS l)

-- Digestive-functor environment builder
paramsToEnv :: Monad m => [P.Param] -> Path -> m [FormInput]
paramsToEnv ((k, v):rest) p =
    if  T.decodeUtf8 k == fromPath p
        then return [TextInput $ T.decodeUtf8 v]
        else paramsToEnv rest p
paramsToEnv _ _ = fail "Parameter not found"


-- User auth related
hashPassword :: Text -> ByteString -> Text
hashPassword pass salt = T.decodeUtf8 . digestToHexByteString . md5 $
    T.encodeUtf8 pass <> salt
  where
    md5 :: ByteString -> Digest MD5
    md5 = hash

verifyUser :: Text -> Text -> ActionT '[Session Text IO, Persist, Logger] prms IO Bool
verifyUser name password = do
    (runSql . get $ SUserKey name) >>= \case
        Just (SUser _ salt pwdHash _ _) ->
            return $ pwdHash == hashPassword password salt
        Nothing -> return False

-- Session shortcut
getSession' :: ActionT '[Session Text IO, Persist, Logger] prms IO SessionInfo
getSession' = getSession pText

-- others
textShow :: Show a => a -> Text
textShow = T.pack . show

decodeJsonText :: (FromJSON a, ToJSON a) => Text -> Maybe a
decodeJsonText =  JSON.decode . fromStrict . T.encodeUtf8
