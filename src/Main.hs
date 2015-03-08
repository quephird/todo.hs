import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (runMigration)
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import Web.Scotty (get, param, middleware, redirect, scotty)

import Todo.Models.Post
import Todo.Views.Post

main = do
  runDb $ runMigration migrateAll
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/create" $ do
      _title <- param "title"
      _content <- param "content"
      now <- liftIO getCurrentTime
      savePost _title _content now
      redirect "/"

    get "/" $ do
      renderList
