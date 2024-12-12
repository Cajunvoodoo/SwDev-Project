{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Bazaar.Player.Reachability where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Void (Void)
import Effectful.Network (Socket)

-- | The purity of the evaluation context. Useful for distinguishing a structure
-- by the evaluation context; a pure version of a datatype may not require
-- a field, or may be better fit by a field of a different type. @Purity@ enables
-- this use case when used as a Datakind.
--
-- NOTE: It is convention to use @p@ as the type variable for 'Purity'.
data Pure

data Impure

-- | The 'Reachability' data family is used to represent the means with which
-- to communicate with Players in 'Pure' and 'Impure' contexts. However, it is
-- polykinded, so instances can be created as-necessary to facilitate new modes
-- of interaction. New instances should derive 'Eq', 'Show', and 'HasName'.
data family Reachability (p :: Type)

newtype instance Reachability Pure = PureComm Text
data instance Reachability Impure = ImpureComm Socket Text
data instance Reachability Void = EmptyReachability

deriving instance Eq (Reachability Pure)
deriving instance Eq (Reachability Impure)
deriving instance Ord (Reachability Pure)
deriving instance Show (Reachability Pure)
deriving instance Show (Reachability Impure)
deriving instance Show (Reachability Void)

-- | Typeclass for types with canonical names.
class HasName a where
  -- | Get the name associated with the value.
  getName :: a -> Text

instance HasName (Reachability Pure) where
  getName (PureComm name) = name

instance HasName (Reachability Impure) where
  getName (ImpureComm _ name) = name

-- | Typeclass for values that can given a 'Void' inner representation.
-- For example, Reachabilities are often void when they must be deserialized,
-- as a placeholder value would otherwise be required.
class Strippable f where
  -- | Strip the inner value, changing it to 'Void'.
  strip :: f a -> f Void
