module Todo.Views.Post where

import Prelude hiding (div, head, id)

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Format (formatTime)
import Database.Persist.Sql (entityVal)
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html5 ((!), body, button, div, form, h1, head, html, img,
                         input, li, link, table, td, th, toHtml, tr, ul)
import Text.Blaze.Html5.Attributes (action, href, id, name, rel, src, type_)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S (html)

import Todo.Models.Post

blaze = S.html . renderHtml

renderTop =
  html $ do
    head $ do
      link ! rel "stylesheet" ! type_ "text/css" ! href "styles/todo.css"
    body $ do
      h1 "My todo list"

renderForm = do
  div ! id "newTodoForm" $ do
    form ! action "/create" $ do
      input ! id "newTitle" !  name "title" ! type_ "text"
      input ! id "newContent" ! name "content" ! type_ "text"
      button "Make todo!" ! type_ "submit"

renderPost post = do
  tr $ do
    td $ (toHtml title)
    td $ (toHtml content)
    td $ (toHtml createdAt)
    td $ do
      if done
        then img ! src "/images/check-icon.png"
        else "" where
          title = postTitle post
          content = postContent post
          createdAt = (formatTime defaultTimeLocale "%B %e, %Y %H:%M:%S") $ postCreatedAt post
          done = postDone post

renderPosts posts = do
  table $ do
    tr $ do
      th $ "Title"
      th $ "Description"
      th $ "Date created"
    forM_ posts $ \post -> renderPost post

renderList = do
  _posts <- liftIO readPosts
  let posts = map entityVal _posts
  blaze $ do
    renderTop
    renderForm
    renderPosts posts
