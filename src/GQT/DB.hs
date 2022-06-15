{-# LANGUAGE OverloadedStrings #-}

module GQT.DB
  ( testConnStr
  , makeTestPool
  , makeDbConnection
  , closeDbConnection
  ) where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger           ( runNoLoggingT
                                                , runStdoutLoggingT
                                                )
import           Control.Monad.Reader           ( MonadIO )
import qualified Data.ByteString.Char8         as BS8
import           Database.PostgreSQL.Simple     ( Connection
                                                , close
                                                , connectPostgreSQL
                                                )
import           GQT.Config                     ( Config(..)
                                                , Environment(..)
                                                )
type ConnectionString = BS8.ByteString

connStrFromConnection :: Environment -> ConnectionString
connStrFromConnection env =
  BS8.pack $ "dbname=gqt" <> suffix <> " user=postgres password=postgres"
 where
  suffix = case env of
    Development -> "_dev"
    Test        -> "_test"

testConnStr :: ConnectionString
testConnStr = BS8.pack "dbname=postgres user=postgres password=postgres"

makeDbConnection :: Environment -> IO Connection
makeDbConnection env = connectPostgreSQL (connStrFromConnection env)

closeDbConnection :: Connection -> IO ()
closeDbConnection = close

-- Creates a pool of a specific db on test so we can do actions without opening a database we'll be using.
-- For example, dropping a database is not quite supported if the database is open.
makeTestPool :: IO Connection
makeTestPool = connectPostgreSQL testConnStr
