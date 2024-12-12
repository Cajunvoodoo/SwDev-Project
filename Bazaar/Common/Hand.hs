module Bazaar.Common.Hand
  ( Hand
  ) where

import Bazaar.Common.Internal.Prelude
import Bazaar.Common.Pebbles
import Data.Aeson (FromJSON (..))
import GHC.IsList ( IsList(Item, fromList, toList) )

-- | Hands are the collection of 'Pebble's Players have.
newtype Hand = Hand
  { hand :: MultiSet Pebble
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromPebbleSet, ToPebbleSet, Semigroup, Monoid)

instance FromJSON Hand where
  parseJSON j = fromPebbleSet <$> parseJSON @PebbleSet j

instance ToJSON Hand where
  toJSON = toJSON . toPebbleSet

instance IsList Hand where
  type Item Hand = Pebble
  toList = convert
  fromList = convert
