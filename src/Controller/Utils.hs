{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller.Utils where

import           Control.Monad.Apiary.Action
import           Control.Monad.Apiary.Filter.Capture
import           Crypto.Hash
import qualified Data.Aeson                          as JSON
import           Data.ByteString                     (ByteString)
import qualified Data.CaseInsensitive                as CI
import           Data.Monoid
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Database.Persist.Postgresql
import           Lucid
import           Model
import qualified Network.Wai.Parse                   as P
import           Text.Digestive.Form.Encoding
import           Text.Digestive.Types
import           TextShow                            as T
import           TextShow.Instances
import           View.NotFound
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
lucidRes l = contentType "text/html" >> lazyBytes (renderBS l)

-- Digestive-functor environment builder
paramsToEnv :: Monad m => [P.Param] -> Path -> m [FormInput]
paramsToEnv ((k, v):rest) p =
    if  T.decodeUtf8 k == fromPath p
        then return [TextInput $ T.decodeUtf8 v]
        else paramsToEnv rest p
paramsToEnv _ _ = fail "Parameter not found"

mkFormEnv :: FormEncType -> ActionT exts prms IO (Path -> ActionT exts prms IO [FormInput])
mkFormEnv _ = getReqBodyParams >>= return . paramsToEnv

-- User auth related
hashPassword :: Text -> ByteString -> Text
hashPassword pass salt = T.decodeUtf8 . digestToHexByteString . sha256 $
    T.encodeUtf8 pass <> salt
  where
    sha256 :: ByteString -> Digest SHA256
    sha256 = hash

verifyUser :: (Has SessionExt exts, Has Persist exts)
    => Text -> Text -> ActionT exts prms IO Bool
verifyUser name password =
    (runSql . get $ SUserKey name) >>= \case
        Just (SUser _ salt pwdHash _ _) ->
            return $ pwdHash == hashPassword password salt
        Nothing -> return False

-- Session shortcut
getSession' :: Has SessionExt exts => ActionT exts prms IO SessionInfo
getSession' = getSession pText

getHeader :: ByteString -> ActionT exts prms IO (Maybe ByteString)
getHeader name = do
    headers <- getHeaders
    return $ lookup (CI.mk name) headers

-- others
textShow :: TextShow a => a -> Text
textShow = showt
