module Main (main) where

import Control.Monad.Logger          (runStdoutLoggingT)
import Database.Persist.Postgresql   (withPostgresqlPool)
import Database.Persist.Sql          (runSqlPool)
import Model                         (migrateAll)
import Network.Wai.Handler.Warp      (run)
import Yesod.Core                    (toWaiApp)

import Foundation (App (..))

-- | Connection string — override with env vars or a config file in production.
connStr :: String
connStr = "host=localhost port=5432 user=survey password=survey dbname=survey"

main :: IO ()
main = runStdoutLoggingT $
  withPostgresqlPool (fromString connStr) 10 $ \pool -> liftIO $ do
    runSqlPool (runMigration migrateAll) pool
    app <- toWaiApp (App pool)
    run 3000 app
  where
    fromString = id
