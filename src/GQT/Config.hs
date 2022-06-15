module GQT.Config
  ( Config(..)
  , Environment(..)
  ) where

import           Database.PostgreSQL.Simple     ( Connection )

newtype Config = Config { configPool :: Connection }

data Environment = Test | Development deriving (Show, Read)
