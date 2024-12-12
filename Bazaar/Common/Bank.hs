module Bazaar.Common.Bank
  ( Bank
  ) where

import Bazaar.Common.Internal.Prelude
import Bazaar.Common.Pebbles
import Data.Aeson (FromJSON (..))
import GHC.IsList ( IsList(Item, fromList, toList) )

-- | Banks are the collection of 'Pebble's stored in the 'GameState'.
newtype Bank = Bank
  { bank :: MultiSet Pebble
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromPebbleSet, ToPebbleSet, Semigroup, Monoid)

instance FromJSON Bank where
  parseJSON j = fromPebbleSet <$> parseJSON @PebbleSet j

instance ToJSON Bank where
  toJSON = toJSON . toPebbleSet

instance IsList Bank where
  type Item Bank = Pebble
  toList = convert
  fromList = convert
