{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (entityVal, insert, runMigration)
import Text.Blaze.Html5 (li, toHtml, ul)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty (get, html, param, redirect, scotty)

import Todo.Models.Post

blaze = html . renderHtml

main = do
  runDb $ runMigration migrateAll
  scotty 3000 $ do
    get "/create/:title" $ do
      _title <- param "title"
      now <- liftIO getCurrentTime
      liftIO $ runDb $ insert $ Post _title "some content" now
      redirect "/"

    get "/" $ do
      _posts <- liftIO readPosts
      let posts = map (postTitle . entityVal) _posts
      blaze $ do
        ul $ do
          forM_ posts $ \post -> li (toHtml post)
