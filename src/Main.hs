import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (runMigration)
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import Web.Scotty (Parsable, delete, get, param, middleware, post, redirect, scotty)

import Todo.Models.Post
import Todo.Views.Post

main = do
  runDb $ runMigration migrateAll
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/todos" $ do
      renderList

    post "/todo" $ do
      _title <- param "title"
      _content <- param "content"
      now <- liftIO getCurrentTime
      savePost _title _content now
      redirect "/todos"

    post "/todo/mark_as_done" $ do
      _id <- param "id"
      now <- liftIO getCurrentTime
      markPostAsDone _id now
      redirect "/todos"

    get "/todo/delete" $ do
      _id <- param "id"
      deletePost _id
      redirect "/todos"
