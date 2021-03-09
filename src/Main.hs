{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Main where

import Data.Acquire (Acquire, ReleaseType(..), mkAcquireType, with, allocateAcquire)
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
import Debug.Trace
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import qualified UnliftIO.Exception as UE
import qualified Data.Text as T


loggedClose :: PGS.Connection -> IO ()
-- TODO replicate other stuff with io ref and stmtFinalize done in persistent:
-- https://github.com/yesodweb/persistent/blob/f82154f80d3eda99c26acfb27d1b391708440580/persistent/Database/Persist/Sql/Run.hs#L302
-- stmtFinalize just does `return ()`, see:
-- https://github.com/yesodweb/persistent/blob/f69716dcfeca01896ec42dec874393fbe60d3939/persistent-postgresql/Database/Persist/Postgresql.hs#L1699
-- TODO more faithfully reproduce this without error
-- which I feel could mess up this whole test
loggedClose conn = PGS.close conn `UE.catchAny` \e -> error "error closing db"


-- TODO try removing this IORef call and seeing if that fixes persistent issue
-- https://github.com/yesodweb/persistent/blob/f82154f80d3eda99c26acfb27d1b391708440580/persistent/Database/Persist/Sql/Run.hs#L304

main :: IO ()
main = do

  -- I started a postgres server with:
  -- docker run --rm --name some-postgres -p 5432:5432 -e POSTGRES_PASSWORD=secret postgres

  -- TODO make createPool like  https://github.com/yesodweb/persistent/blob/f82154f80d3eda99c26acfb27d1b391708440580/persistent/Database/Persist/Sql/Run.hs#L230
  -- TODO could runLoggingT have anything to do with masking/async exception weirdness?
  -- pool config have anything to do with it?
  -- what about the function to create a connection that's passed in?
  pool <- Pool.createPool (PGS.connect PGS.defaultConnectInfo { PGS.connectPassword = "secret" } ) loggedClose 1 10 1


  Pool.withResource pool $ \conn -> do
    PGS.execute_ conn "create table if not exists foo(id int);"

  let freeConn (res, localPool) relType =
        case (("relType: " <> show relType) `trace` relType) of
          ReleaseException -> Pool.destroyResource pool localPool res
          _ -> Pool.putResource localPool res

  let acq = fst <$> mkAcquireType (Pool.takeResource pool) freeConn

  threadId <- Concurrent.forkIO $ do

      -- TODO simulate what persistent does
      -- https://github.com/codygman/persistent/blob/3d34fed5f2958c1e2071d2846ec0dd785dacb69d/persistent/Database/Persist/Sql/Run.hs#L46

    Monad.void . with acq $ \conn -> do
      liftIO $ PGS.execute_ conn "insert into foo values(1);"
      let numThings :: Int = 100000000
      putStrLn $ "start inserting " <> show numThings <> " things"
      Monad.forM_ [1 .. numThings] $ \_ ->  do
        PGS.execute_ conn "insert into foo values(1);"


  putStrLn "waiting for insert thread to make progress"
  Concurrent.threadDelay 5000000
  Monad.void $ Concurrent.killThread threadId
  putStrLn "killing insert thread"

  Monad.void . with acq $ \conn -> do
      liftIO $ PGS.execute_ conn "insert into foo values(1);"

  putStrLn "done"
