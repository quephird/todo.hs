module Todo.Models.Post where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Entity, Key, SelectOpt(LimitTo), (==.), deleteWhere, selectList)
import Database.Persist.Sql (SqlBackend, ToBackendKey, delete, insert, toSqlKey)
import Database.Persist.Sqlite (SqlPersistT, runSqlConn, withSqliteConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Types (PersistValue(PersistInt64))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post
    title String
    content Text
    createdAt UTCTime
    done Bool
    deriving Show
|]

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb query = runNoLoggingT . runResourceT . withSqliteConn "dev.sqlite3" . runSqlConn $ query

readPosts :: IO [Entity Post]
readPosts = (runDb $ selectList [] [LimitTo 10])

savePost ::  MonadIO m => String -> Text -> UTCTime -> m (Key Post)
savePost title content time = liftIO $ runDb $ insert $ Post title content time False

toKey :: ToBackendKey SqlBackend a => Integer -> Key a
toKey i = toSqlKey $ fromIntegral (i :: Integer)

deletePost :: MonadIO m => Integer -> m ()
deletePost id_ = liftIO $ runDb $ delete (toKey id_ :: PostId)
