{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Controller.Doc
import           Controller.Root
import           Controller.Search
import           Controller.Snippet
import           Controller.User
import           Controller.Utils

import           Data.Serialize.Text              ()
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Database.Persist.Postgresql
import           Model
import           Network.Wai.Handler.Warp         (run)
import           System.Environment               (getArgs, lookupEnv)
import           Text.Read                        (readMaybe)
import           Web.Apiary
import           Web.Apiary.Database.Persist
import           Web.Apiary.Logger
import           Web.Apiary.Session.ClientSession

main :: IO ()
main = getArgs >>= parseArgs
  where
    parseArgs ["-h"] = usage
    parseArgs ["-v"] = version
    parseArgs [connStr] = lookupEnv "PORT" >>= return . (>>= readMaybe) >>= \case
        Just port -> startServer port connStr
        Nothing -> startServer 80 connStr
    parseArgs _ = usage

    usage   = putStrLn "Usage: jsmServer [-vh]"
           >> putStrLn "       PORT=XX jsmServer postgreSqlConnectString"
    version = putStrLn "jsmServer v0.1"

startServer :: Int -> FilePath -> IO ()
startServer port connStr = do
    putStr "jsmServer started @" >> print port
    runApiaryWith (run port)
        (initLogger def
            +> initPersistPool (withPostgresqlPool (T.encodeUtf8 $ T.pack connStr) 10) migrateAll
            +> initClientSession pText def{
                    csCookieName = "jsm_acess_sess"
                ,   csCookieSecure = False
                ,   csTTL = Just 3600
                }
        )
        def $ do
            createKeywordIndex

            rootRouter
            userRouter
            searchRouter
            snippetRouter
            documentRouter
            notFound404Router

  where
    createKeywordIndex = runSql $
        rawSql "SELECT count(*) from pg_class where relname='snippet_keywords_idx'" [] >>= \case
            [Just s] ->
                if unSingle s == PersistInt64 0
                    then do
                        rawExecute
                            "CREATE INDEX snippet_keywords_idx ON snippet USING gin (keywords)" []
                        liftIO $ putStrLn "Creating index on snippet keywords."
                    else liftIO $ putStrLn "Index exists, skip create index on snippet keywords."
            _ -> fail "Fail to check index, check you database."
