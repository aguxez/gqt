{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.ByteString.Lazy.Char8     ( ByteString )
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Types            ( GQLType(..)
                                                , RootResolver(..)
                                                , Undefined(..)
                                                )
import           Data.Pool                      ( Pool
                                                , PoolConfig(..)
                                                , newPool
                                                )
import qualified Data.Text.Lazy.IO             as TIO
import           Database.PostgreSQL.Simple     ( Connection )
import           GHC.Generics                   ( Generic )
import           GQT.Config                     ( Environment(..) )
import           GQT.DB                         ( closeDbConnection
                                                , makeDbConnection
                                                )
import           GQT.GraphQL.Types.Deity        ( Deity(..)
                                                , resolveDeities
                                                )
import           Web.Scotty                     ( body
                                                , get
                                                , html
                                                , post
                                                , raw
                                                , scotty
                                                )

newtype Query m = Query { deities :: m [Deity] }
  deriving (Generic, GQLType)

rootResolver :: Pool Connection -> RootResolver IO () Query Undefined Undefined
rootResolver pool = RootResolver
  { queryResolver        = Query { deities = resolveDeities pool }
  , mutationResolver     = Undefined
  , subscriptionResolver = Undefined
  }

gqlAPI :: Pool Connection -> ByteString -> IO ByteString
gqlAPI pool = interpreter (rootResolver pool)

main :: IO ()
main = do
  let poolConfig = PoolConfig { createResource   = makeDbConnection Development
                              , freeResource     = closeDbConnection
                              , poolCacheTTL     = 1
                              , poolMaxResources = 50
                              }
  pool <- newPool poolConfig
  scotty 4040 $ do
    get "/" $ html =<< liftIO (TIO.readFile "static/index.html")

    post "/graphql" $ do
      b   <- body
      res <- liftIO $ gqlAPI pool b
      raw res
