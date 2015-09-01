{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Monad
import           Control.Monad.Apiary.Action
import           Controller
import           Data.Serialize.Text              ()
import qualified Data.Text                        as T
import           Database.Persist.Sqlite
import           Model
import           Network.Wai.Handler.Warp         (run)
import           System.Environment               (getArgs, lookupEnv)
import           System.IO
import           System.IO.Error
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
    parseArgs [dbfile] = do
        lookupEnv "PORT" >>= return . (>>= readMaybe) >>= \case
            Just port -> startServer port dbfile
            Nothing -> startServer 80 dbfile
    parseArgs [] = parseArgs ["jsm.db"]
    parseArgs _ = usage

    usage   = putStrLn "Usage: jsmServer [-vh]"
           >> putStrLn "       PORT=XX jsmServer dbfile"
    version = putStrLn "jsmServer v0.1"

startServer :: Int -> FilePath -> IO ()
startServer port dbfile = do
    putStr "jsmServer started @" >> print port
    runApiaryWith (run port)
        (initLogger def
            +> initPersistPool (withSqlitePool (T.pack dbfile) 10) migrateAll
            +> initClientSession pText def{
                    csCookieName = "jsm_acess_sess"
                ,   csCookieSecure = False
                ,   csTTL = Just 3600
                }
        )
        def $ do
            rootRouter
            userRouter
            snippetRouter
            commentRouter
            notFound404Router
