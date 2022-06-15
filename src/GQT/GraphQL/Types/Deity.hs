{-# LANGUAGE OverloadedStrings #-}

module GQT.GraphQL.Types.Deity
  ( Deity(..)
  , resolveDeities
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..)
                                                , liftIO
                                                )
import           Data.Map.Lazy                  ( fromList )
import           Data.Morpheus.Types            ( GQLType(..)
                                                , ResolverQ
                                                )
import           Data.Pool                      ( Pool
                                                , withResource
                                                )
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple     ( Connection )
import           GHC.Generics                   ( Generic )
import           GQT.Deity                      ( Deity(..)
                                                , deitySelect
                                                , runDeitySelect
                                                )

instance GQLType Deity where
  description =
    const (Just "A supernatural being considered divine and sacred")
  getDescriptions = const
    (fromList
      [ ("fullName", "The full name of the deity")
      , ("power"   , "The power of the deity")
      , ("reign"   , "The reign of the deity")
      ]
    )

resolveDeities :: Pool Connection -> ResolverQ () IO [Deity]
resolveDeities pool =
  liftIO $ withResource pool $ \conn -> runDeitySelect conn deitySelect

