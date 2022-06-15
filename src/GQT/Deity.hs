{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module GQT.Deity
  ( runDeitySelect
  , deitySelect
  , Deity(..)
  ) where

import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                , Applicative
                                                )
import qualified Data.Profunctor               as P
import           Data.Profunctor.Product        ( p3 )
import qualified Data.Profunctor.Product.Default
                                               as D
import           Data.Profunctor.Product.Default
                                                ( Default )
import           Data.Text                      ( Text )
import qualified Database.PostgreSQL.Simple    as PGS
import           GHC.Generics                   ( Generic )
import           Opaleye                        ( Field
                                                , Select
                                                , SqlText
                                                , Table
                                                , Unpackspec
                                                , runSelect
                                                , selectTable
                                                , table
                                                , tableField
                                                )
import qualified Opaleye                       as O

data DeityField = DeityField
  { deityFullNameField :: Field SqlText
  , deityPowerField    :: Field SqlText
  , deityReignField    :: Field SqlText
  }

data Deity = Deity
  { deityFullName :: Text
  , deityPower    :: Text
  , deityReign    :: Text
  }
  deriving Generic

deityFieldDef
  :: ( Applicative (p DeityField)
     , P.Profunctor p
     , Default p (Field SqlText) (Field SqlText)
     )
  => p DeityField DeityField

deityFieldDef =
  DeityField
    <$> P.lmap deityFullNameField D.def
    <*> P.lmap deityPowerField D.def
    <*> P.lmap deityReignField D.def

instance Default Unpackspec DeityField DeityField where
  def = deityFieldDef

instance Default O.FromFields DeityField Deity where
  def =
    Deity
      <$> P.lmap deityFullNameField D.def
      <*> P.lmap deityPowerField D.def
      <*> P.lmap deityReignField D.def

deitiesTable :: Table DeityField DeityField
deitiesTable = table
  "deities"
  (   DeityField
  <$> P.lmap deityFullNameField (tableField "full_name")
  <*> P.lmap deityPowerField (tableField "power")
  <*> P.lmap deityReignField (tableField "reign")
  )

deitySelect :: Select DeityField
deitySelect = selectTable deitiesTable

runDeitySelect :: PGS.Connection -> Select DeityField -> IO [Deity]
runDeitySelect = runSelect
