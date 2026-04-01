module Main (main) where

import Control.Monad.IO.Class        (liftIO)
import Control.Monad.Logger          (runStdoutLoggingT)
import Data.ByteString               (ByteString)
import Database.Persist.Postgresql   (withPostgresqlPool)
import Database.Persist.Sql          (runSqlPool, runMigration)
import Network.Wai.Handler.Warp      (run)
import Yesod.Core                    (toWaiApp)

import Foundation                    (App (..))
import Model                         (migrateAll)

connStr :: ByteString
connStr = "host=localhost port=5432 user=survey password=survey dbname=survey"

main :: IO ()
main = runStdoutLoggingT $
  withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    runSqlPool (runMigration migrateAll) pool
    app <- toWaiApp (App pool)
    run 3000 app
