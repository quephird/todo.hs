{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Todo.Models.Post where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Entity, selectList, SelectOpt(LimitTo))
import Database.Persist.Class (Key)
import Database.Persist.Sql (insert)
import Database.Persist.Sqlite (runSqlConn, SqlPersist, withSqliteConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post
    title String
    content Text
    createdAt UTCTime
    deriving Show
|]

runDb :: SqlPersist (ResourceT (NoLoggingT IO)) a -> IO a
runDb query = runNoLoggingT . runResourceT . withSqliteConn "dev.sqlite3" . runSqlConn $ query

readPosts :: IO [Entity Post]
readPosts = (runDb $ selectList [] [LimitTo 10])

savePost ::  MonadIO m => String -> Text -> UTCTime -> m (Key Post)
savePost title content time = liftIO $ runDb $ insert $ Post title content time
