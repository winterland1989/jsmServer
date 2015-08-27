{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Controller
import           Data.Serialize.Text              ()
import qualified Data.Text                        as T
import           Database.Persist.Sqlite
import           Model
import           Network.Wai.Handler.Warp         (run)
import           System.Environment               (getArgs)
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
    parseArgs [port] = parseArgs [port, "jsm.db"]
    parseArgs [port, dbfile] = case readMaybe port of
        Just port' -> startServer port' dbfile
        Nothing -> usage
    parseArgs _ = usage

    usage   = putStrLn "Usage: jsmServer [-vh]"
           >> putStrLn "       jsmServer port"
           >> putStrLn "       jsmServer port dbfile"
    version = putStrLn "jsmServer v0.1"

startServer :: Int -> FilePath -> IO ()
startServer port dbfile = runApiaryWith (run port)
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
