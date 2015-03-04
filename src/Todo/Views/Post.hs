{-# LANGUAGE OverloadedStrings #-}

module Todo.Views.Post where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (entityVal)
import Text.Blaze.Html5 (body, h1, html, li, toHtml, ul)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S (html)

import Todo.Models.Post

blaze = S.html . renderHtml

renderPosts = do
  _posts <- liftIO readPosts
  let posts = map (postTitle . entityVal) _posts
  blaze $ do
    html $ do
      body $ do
        h1 "My todo list"
        ul $ do
          forM_ posts $ \post -> li (toHtml post)
