{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Main where

import  qualified Control.Monad as Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Database.Persist as Persist
import qualified Database.Persist.Sql as Persist
import qualified Database.Persist.Postgresql as Persist
import qualified Control.Monad.Logger as Logger
import qualified Data.ByteString as BS
import qualified Data.Pool as Pool
import Data.Time
import qualified Database.PostgreSQL.Simple as PGS

main :: IO ()
main = do

  -- I started a postgres server with:
  -- docker run --rm --name some-postgres -p 5432:5432 -e POSTGRES_PASSWORD=secret postgres

  pool <- Pool.createPool (PGS.connect PGS.defaultConnectInfo { PGS.connectPassword = "secret" } ) PGS.close 1 10 1


  Pool.withResource pool $ \conn -> do
    PGS.execute_ conn "create table if not exists foo(id int);"


  threadId <- Concurrent.forkIO $ do
      Pool.withResource pool $ \conn -> do
        let numThings :: Int = 100000000
        putStrLn $ "start inserting " <> show numThings <> " things"
        Monad.forM_ [1 .. numThings] $ \_ ->  do
          PGS.execute_ conn "insert into foo values(1);"


  putStrLn "waiting for insert thread to make progress"
  Concurrent.threadDelay 5000000
  Monad.void $ Concurrent.killThread threadId
  putStrLn "killing insert thread"

  Pool.withResource pool $ \conn -> do
    PGS.execute_ conn "insert into foo values(1);"

  putStrLn "done"
