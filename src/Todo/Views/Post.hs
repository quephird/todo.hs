{-# LANGUAGE OverloadedStrings #-}

module Todo.Views.Post where

import Prelude hiding (head)

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Format (formatTime)
import Database.Persist.Sql (entityVal)
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html5 ((!), body, h1, head, html, li, link, table, td, th, toHtml, tr, ul)
import Text.Blaze.Html5.Attributes (href, rel, type_)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S (html)

import Todo.Models.Post

blaze = S.html . renderHtml

renderPost post = do
  tr $ do
    td $ (toHtml title)
    td $ (toHtml content)
    td $ (toHtml createdAt) where
      title = postTitle post
      content = postContent post
      createdAt = (formatTime defaultTimeLocale "%B %e, %Y %H:%M:%S") $ postCreatedAt post

renderPosts = do
  _posts <- liftIO readPosts
  let posts = map entityVal _posts
  blaze $ do
    html $ do
      head $ do
        link ! rel "stylesheet" ! type_ "text/css" ! href "styles/todo.css"
      body $ do
        h1 "My todo list"
        table $ do
          tr $ do
            th $ "Title"
            th $ "Description"
            th $ "Date created"
          forM_ posts $ \post -> renderPost post
