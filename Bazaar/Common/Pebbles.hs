{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Defines the fundamental game piece of Pebbles.
module Bazaar.Common.Pebbles
  ( Pebble (..)
  , PebbleSet
  , ToPebbleSet (..)
  , FromPebbleSet (..)
  , isSubset
  , difference
  , union
  , intersection
  , exchangePebbleSet
  , mkRandomPebbleVec
  , mkRandomPebble
  , mkRandomPebbleSet
  , convert
  , isStrictSubset
  , pickRandomPebble
  , isEmpty
  , sizePS
  , pebbles
  , nextPbl
  , concatPs
  ) where

import Bazaar.Common.Draw
import Bazaar.Common.Internal.Prelude
import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.TH
import Data.Char qualified as Char
import Data.Maybe (fromJust)
import Data.MultiSet qualified as MS
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Data.Vector.Sized qualified as Sized
import Diagrams.Prelude (fc, lc, (#))
import Diagrams.Prelude qualified as Diagram
import System.Random qualified as Random
import Data.List.Extra (foldl1')

-- * Pebbles

-- | Fundamental game piece, used as the currency in Bazaar.
--
-- Pebbles are defined alphabetically; a Pebble is compared lexicographically.
data Pebble
  = BluePbl
  | GreenPbl
  | RedPbl
  | WhitePbl
  | YellowPbl
  deriving stock (Eq, Ord, Enum, Bounded, Show)

-- | Every pebble in alphabetical order.
pebbles :: [Pebble]
pebbles = enumFrom (minBound @Pebble)

-- | Like 'succ', but not partial.
nextPbl :: Pebble -> Pebble
nextPbl YellowPbl = BluePbl
nextPbl pbl = succ pbl

$( deriveJSON
    defaultOptions
      { sumEncoding = UntaggedValue
      , constructorTagModifier =
          -- note: the fromJust happens at /compile time/, never runtime.
          fmap Char.toLower . T.unpack . fromJust . T.stripSuffix "Pbl" . T.pack
      }
    ''Pebble
 )

instance
  ( TrailLike t
  , V t ~ V2
  , N t ~ Double
  , Transformable t
  , HasStyle t
  )
  => Draw Pebble t
  where
  draw peb = Diagram.circle 1 # fc (pebbleColor peb) # lc Diagram.black

-- | Convert a 'Pebble' into its respective 'Diagram.Colour'.
pebbleColor :: Pebble -> Diagram.Colour Double
pebbleColor = \case
  RedPbl -> Diagram.red
  BluePbl -> Diagram.blue
  GreenPbl -> Diagram.green
  WhitePbl -> Diagram.white
  YellowPbl -> Diagram.yellow

-- | Given a source of randomness, generate a random 'Pebble'.
mkRandomPebble :: (RandomGen g) => g -> (Pebble, g)
mkRandomPebble = first toEnum . Random.uniformR (0, fromEnum @Pebble maxBound)

-- * PebbleSets

-- | Generic container for a collection of 'Pebble's.
-- Used for operations between collections of Pebbles with different
-- interpretations.
newtype PebbleSet = PebbleSet (MultiSet Pebble)
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | A 'PebbleSet' is first compared by size, then by element.
instance Ord PebbleSet where
  PebbleSet ms `compare` PebbleSet ms'
    | length ms == length ms' = compare ms ms' -- element-wise
    | otherwise = compare (length ms) (length ms')

instance FromJSON PebbleSet where
  parseJSON (A.Array pebsA) = do
    (pebs :: Vector Pebble) <- mapM A.parseJSON pebsA
    pure $ toPebbleSet pebs
  parseJSON _ = fail "Incorrect type to construct a Pebbleset."

instance ToJSON PebbleSet where
  toJSON (PebbleSet ms) = toJSON $ MS.toList ms

instance (DrawConstraints t b) => Draw PebbleSet t where
  draw (PebbleSet ms) = Diagram.hsep 0.25 $ draw <$> MS.toList ms

-- | Conversion for types with a canonical representation as a 'PebbleSet'.
--
-- Laws: if @a@ is an instance of 'FromPebbleSet', it is expected that
-- @fromPebbleSet . toPebbleSet ≡ id@.
class ToPebbleSet a where
  toPebbleSet :: a -> PebbleSet

-- | Conversion for types with a canonical conversion from a 'PebbleSet' to @a@.
--
-- Laws: if @a@ is an instance of 'ToPebbleSet', it is expected that
-- @fromPebbleSet . toPebbleSet ≡ id@.
class FromPebbleSet a where
  fromPebbleSet :: PebbleSet -> a

instance ToPebbleSet PebbleSet where
  toPebbleSet = coerce

instance FromPebbleSet PebbleSet where
  fromPebbleSet = coerce

instance ToPebbleSet Pebble where
  toPebbleSet = coerce . MS.singleton

instance (ToPebbleSet a, Foldable f) => ToPebbleSet (f a) where
  toPebbleSet = foldMap toPebbleSet

instance FromPebbleSet [Pebble] where
  fromPebbleSet = MS.toList . coerce

instance FromPebbleSet (MultiSet Pebble) where
  fromPebbleSet = coerce

instance FromPebbleSet (Vector Pebble) where
  fromPebbleSet = Vector.fromList . convert @[Pebble]

-- | Internal function that converts to then unwraps from a 'PebbleSet'.
unPebbleSet :: (ToPebbleSet a) => a -> MultiSet Pebble
unPebbleSet = coerce . toPebbleSet

-- | Convert some @a@ to some @b@, using 'PebbleSet' as an intermediate
-- representation.
convert :: forall b a. (ToPebbleSet a, FromPebbleSet b) => a -> b
convert = fromPebbleSet . toPebbleSet

-- | Is the first argument a subset of the second?
isSubset :: (ToPebbleSet a, ToPebbleSet b) => a -> b -> Bool
isSubset a b = unPebbleSet a `MS.isSubsetOf` unPebbleSet b

-- | Is the first argument a strict subset of the second?
isStrictSubset :: (ToPebbleSet a, ToPebbleSet b) => a -> b -> Bool
isStrictSubset a b = unPebbleSet a `MS.isProperSubsetOf` unPebbleSet b

-- | Calculate the difference between the 'PebbleSet' representations.
difference :: (ToPebbleSet a, ToPebbleSet b) => a -> b -> PebbleSet
difference a b = coerce $ unPebbleSet a `MS.difference` unPebbleSet b

-- | Calculate the union of the 'PebbleSet' representations.
union :: (ToPebbleSet a, ToPebbleSet b) => a -> b -> PebbleSet
union a b = coerce $ unPebbleSet a `MS.union` unPebbleSet b

-- | Like 'union', but operates on lists. Effectively concatenates all 'PebbleSet's
-- in the provided list.
concatPs :: (ToPebbleSet a) => [a] -> PebbleSet
concatPs [] = mempty
concatPs as = foldl1' union (fmap toPebbleSet as)

-- | Calculate the intersection of the 'PebbleSet' representations.
intersection :: (ToPebbleSet a, ToPebbleSet b) => a -> b -> PebbleSet
intersection a b = coerce $ unPebbleSet a `MS.intersection` unPebbleSet b

-- | Is this 'PebbleSet' empty?
isEmpty :: (ToPebbleSet a) => a -> Bool
isEmpty = MS.null . unPebbleSet

-- | Calculate the number of elements in the provided @a@.
sizePS :: (ToPebbleSet a) => a -> Natural
sizePS = toEnum . MS.size . coerce . toPebbleSet

-- | Given a 'PebbleSet' to exchange to/from, the cost of the exchange (as a PebbleSet),
-- and the profit of the exchange (as a PebbleSet), subtract the cost and add the profit.
--
-- Note: @cost@ needn't be present in @src@; this function merely serves as a
-- convenience over 'union' and 'intersection'.
--
-- Example (pseudocode):
-- @
-- >>> exchangePebbleSet {RedPbl, GreenPbl, BluePbl} {large bank} {GreenPbl} {WhitePbl}
-- ({RedPbl, BluePbl, WhitePbl}, {large bank + WhitePbl - GreenPbl})
-- @
exchangePebbleSet
  :: ( ToPebbleSet a
     , ToPebbleSet b
     , ToPebbleSet c
     , ToPebbleSet d
     )
  => a
  -- ^ Source from which to subtract cost and add profit.
  -> b
  -- ^ Bank from which to subtract the profit and add the cost.
  -> c
  -- ^ Cost.
  -> d
  -- ^ Profit.
  -> Maybe (PebbleSet, PebbleSet)
  -- ^ (Resulting source, Resulting bank)
exchangePebbleSet src bank cost profit
  | cost `isSubset` src
  , profit `isSubset` bank =
      Just
        ( union profit $ difference src cost
        , union cost $ difference bank profit
        )
  | otherwise = Nothing

-- | Given a 'PebbleSet', pick a random 'Pebble' from the set, remove it, and
-- return the removed pebble and new 'FromPebbleSet' value. Returns 'Nothing' if
-- the given 'PebbleSet' is empty.
pickRandomPebble
  :: (ToPebbleSet a, FromPebbleSet a, RandomGen g)
  => g
  -> a
  -> Maybe ((Pebble, a), g)
pickRandomPebble g a = (,) <$> ((,) <$> mbPeb <*> a') <*> Just g'
 where
  msPeb = convert @(MultiSet Pebble) a
  setNoZero = MS.toSet $ convert @(MultiSet Pebble) a
  (ix, g') = Random.uniformR (0, max 0 $ Set.size setNoZero - 1) g
  mbPeb =
    if ix == Set.size setNoZero
      then Nothing
      else Just (Set.elemAt ix setNoZero)
  a' = convert . flip MS.delete msPeb <$> mbPeb

-- | Make a 'PebbleSet' of @n@ random 'Pebble's, with possible duplicate elements.
mkRandomPebbleSet
  :: forall n g
   . (RandomGen g, KnownNat n)
  => g
  -> (PebbleSet, g)
mkRandomPebbleSet = first toPebbleSet . mkRandomPebbleVec @n

-- | Make a 'Sized.Vector' of @n@ random 'Pebble's, with possible duplicate elements.
mkRandomPebbleVec
  :: forall n g
   . (RandomGen g, KnownNat n)
  => g
  -> (Sized.Vector n Pebble, g)
mkRandomPebbleVec g = (,fst $ Random.split g) $ Sized.unfoldrN @n mkRandomPebble g
