module Models.Task where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Entity, Key, SelectOpt(LimitTo), (=.), deleteWhere, selectList)
import Database.Persist.Sql (SqlBackend, ToBackendKey, delete, insert, toSqlKey, update)
import Database.Persist.Sqlite (SqlPersistT, runSqlConn, withSqliteConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Types (PersistValue(PersistInt64))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
    title String
    content Text
    createdAt UTCTime
    done Bool
    deriving Show
|]

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb query = runNoLoggingT . runResourceT . withSqliteConn "dev.sqlite3" . runSqlConn $ query

readTasks :: IO [Entity Task]
readTasks = (runDb $ selectList [] [LimitTo 10])

saveTask ::  MonadIO m => String -> Text -> UTCTime -> m (Key Task)
saveTask title content time = liftIO $ runDb $ insert $ Task title content time False

toKey :: ToBackendKey SqlBackend a => Integer -> Key a
toKey i = toSqlKey $ fromIntegral (i :: Integer)

markTaskAsDone :: MonadIO m => Integer -> UTCTime -> m ()
markTaskAsDone id_ time = liftIO $ runDb $ update (toKey id_ :: TaskId) [TaskDone =. True]

deleteTask :: MonadIO m => Integer -> m ()
deleteTask id_ = liftIO $ runDb $ delete (toKey id_ :: TaskId)
