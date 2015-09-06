{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module View.NotFound where

import           Lucid
import           View.Utils

notFoundPage :: Html ()
notFoundPage = doctypehtml_ . html_ $ do
    pageTitle  "Not found | jsm"
    body_ $
        div_
            [style_ "width: 100%; height: 100%; line-height: 100%;\
                \font-size: 100px; text-align: center; margin-top: 200px;" ]
            "404 dude, Get out of here!!!"

