{-# LANGUAGE UndecidableInstances #-}

module Bazaar.Common.Equations
  ( Equation
  , DirectionalEquation
  , EquationTable
  , Equationlike (..)

    -- * Construction
  , mkRandomEquation
  , mkRandomEquationTable
  , mkDirEquation
  , mkEquationTable

    -- * Operations on 'Equation's and 'DirectionalEquation's
  , exchange
  , flipDirEqn
  , splitEquation

    -- * Operations on 'EquationTable's
  , filterTable
  , anySatisfies
  , mapEquationTable
  , mapDirectionally
  , Trades
  , unwrapEquationTable
  , inTable
  ) where

import Bazaar.Common.Draw
import Bazaar.Common.Internal.Prelude
import Bazaar.Common.Pebbles
import Data.Aeson qualified as A
import Data.Bifunctor (bimap)
import Data.List
import Data.MultiSet qualified as MS
import Data.Vector qualified as Vector
import Data.Vector.Sized qualified as Sized
import Diagrams (fc, lc, (#))
import Diagrams qualified as Diagram
import Diagrams.Prelude (def, lw, _shaftStyle)
import Diagrams.Prelude qualified as Diagram
import System.Random qualified as Random

-- | Class describing types that can be converted into LHS and RHS forms, along
-- with checks of satisfiability.
class (Eq eqn) => Equationlike eqn where
  -- | Given a source with a 'PebbleSet', a bank, and an @eqn@, determine whether the
  -- value satisfies either side of the Equation.
  satisfies :: (ToPebbleSet a, ToPebbleSet bank) => a -> bank -> eqn -> Bool

  -- | Convert the left-hand side (lhs) of the @eqn@ to a 'PebbleSet'.
  lhsToPebbleSet :: eqn -> PebbleSet

  -- | Convert the right-hand side (rhs) of the @eqn@ to a 'PebbleSet'.
  rhsToPebbleSet :: eqn -> PebbleSet

  -- | Promote something 'Equationlike' into an 'Equation'.
  promoteEqn :: eqn -> Equation

-- | The Equations used in the game /Bazaar/. Equations are bi-directional
-- formulas that allow players to exchange 'Pebble's for another set of Pebbles.
-- An Equation will never have more than 4 types of Pebbles on each side, and
-- no fewer than 1. Each side has a unique set of pebbles; no duplicate element
-- is present between the sides.
--
-- To convert either side to a 'PebbleSet', use 'lhsToPebbleSet' and 'rhsToPebbleSet',
-- as 'Equation' does not have a single logical instance of 'ToPebbleSet'.
newtype Equation = Equation
  { dirEqn :: DirectionalEquation
  }
  deriving stock (Show)
  deriving newtype (FromJSON)

data DirectionalEquation = DirEqn
  { lhs :: PebbleSet
  , rhs :: PebbleSet
  }
  deriving stock (Show)

instance Ord DirectionalEquation where
  compare (DirEqn lhs rhs) (DirEqn lhs' rhs') =
    case compare lhs lhs' of
      EQ -> compare rhs rhs'
      o -> o

instance Equationlike Equation where
  satisfies src bank (Equation dirEqn) =
    satisfies src bank dirEqn || satisfies src bank (flipDirEqn dirEqn)

  lhsToPebbleSet = lhsToPebbleSet . dirEqn
  rhsToPebbleSet = rhsToPebbleSet . dirEqn

  promoteEqn = id

instance Equationlike DirectionalEquation where
  satisfies src bank DirEqn {lhs, rhs} =
    lhs `isSubset` src && rhs `isSubset` bank

  lhsToPebbleSet = toPebbleSet . lhs
  rhsToPebbleSet = toPebbleSet . rhs

  promoteEqn = Equation

instance Eq Equation where
  (Equation dirEqn) == (Equation dirEqn') =
    dirEqn == dirEqn' || flipDirEqn dirEqn == dirEqn'

instance ToJSON Equation where
  toJSON (Equation {dirEqn = DirEqn {..}}) = toJSON (lhs, rhs)

instance Eq DirectionalEquation where
  (DirEqn lhs rhs) == (DirEqn lhs' rhs') =
    lhs == lhs' && rhs == rhs'

instance FromJSON DirectionalEquation where
  parseJSON (A.Array v) =
    case v of
      [A.Array lhsA, A.Array rhsA] -> do
        (toList -> lhs) <- mapM A.parseJSON lhsA
        (toList -> rhs) <- mapM A.parseJSON rhsA
        case mkDirEquation lhs rhs of
          Nothing -> fail "Invalid elements found while creating Equation."
          Just eqn -> pure eqn
      _ ->
        fail $
          "Too many or too few elements to create an DirectionalEquation."
            <> "DirectionalEquation is malformed."
  parseJSON _invalid = fail "Incorrect type to construct an DirectionalEquation."

instance ToJSON DirectionalEquation where
  toJSON (DirEqn l r) = toJSON (l, r)

instance (DrawConstraints t b) => Draw Equation t where
  draw eqn =
    drawEqnHelper
      eqn
      ( Diagram.vsep
          0.8
          [ Diagram.rect 1.5 0.001 # fc Diagram.black # lc Diagram.black
          , Diagram.rect 1.5 0.001 # fc Diagram.black # lc Diagram.black
          ]
          # Diagram.translateY 0.35
      )

instance (DrawConstraints t b) => Draw DirectionalEquation t where
  draw eqn =
    drawEqnHelper
      eqn
      ( Diagram.arrow' (def {_shaftStyle = _shaftStyle def # lw 0.1}) 0.1
      )

-- | Helper for the 'Draw' instances of 'Equation' and 'DirectionalEquation'.
drawEqnHelper :: (DrawConstraints a b, Equationlike eqn) => eqn -> a -> a
drawEqnHelper eqn sign =
  Diagram.hsep
    0.7
    [ draw (lhsToPebbleSet eqn)
    , sign
    , draw (rhsToPebbleSet eqn)
    ]

-- | Swap the left-hand-side and right-hand-side of a DirectionalEquation.
flipDirEqn :: DirectionalEquation -> DirectionalEquation
flipDirEqn (DirEqn l r) = DirEqn r l

-- | Given a LHS and a RHS, attempt to create an 'DirectionalEquation', failing if the two
-- sides are not disjoint or if either side has more than 4 elements.
mkDirEquation :: [Pebble] -> [Pebble] -> Maybe DirectionalEquation
mkDirEquation lhsList rhsList
  | lhs `intersection` rhs == mempty
  , length lhsList <= 4 && length rhsList <= 4
  , not (null lhsList) && not (null rhsList) =
      Just $ DirEqn (fromPebbleSet lhs) (fromPebbleSet rhs)
  | otherwise = Nothing
 where
  lhs = toPebbleSet lhsList
  rhs = toPebbleSet rhsList

-- | Given the bank, wallet, and an equation to apply, return the application of
-- the 'DirectionalEquation', or 'Nothing' if it cannot be satisfied.
exchange
  :: (ToPebbleSet a, FromPebbleSet a, ToPebbleSet bank, FromPebbleSet bank)
  => bank
  -> a
  -> DirectionalEquation
  -> Maybe (a, bank)
exchange bank wallet (DirEqn {lhs, rhs}) = lhsExchange
 where
  convertBoth = bimap fromPebbleSet fromPebbleSet
  lhsExchange = convertBoth <$> exchangePebbleSet wallet bank lhs rhs

--------------------------

-- * Equation Tables    --

--------------------------

-- | Table of 'Equation's, which contains no duplicate elements.
newtype EquationTable = EquationTable (Vector Equation)
  deriving stock (Show)

instance Eq EquationTable where
  (EquationTable lEqnT) == (EquationTable rEqnT) =
    -- NOTE: if we ever define an Ord instance for 'Equation', we can
    --       asymptotically speed this up.
    foldr (\eqn acc -> eqn `Vector.elem` rEqnT && acc) True lEqnT

-- | Duplicate elements are removed in favor of elements of the left EquationTable.
-- This is lawful under the 'Eq' instance of EquationTable.
instance Semigroup EquationTable where
  lEqnT@(EquationTable lEqns) <> (EquationTable rEqns)
    | Vector.null rEqns = lEqnT
    | Vector.head rEqns `Vector.elem` lEqns =
        lEqnT <> EquationTable (Vector.tail rEqns)
    | otherwise =
        EquationTable (Vector.snoc lEqns $ Vector.head rEqns)
          <> EquationTable (Vector.tail rEqns)

instance Monoid EquationTable where
  mempty = EquationTable []

instance ToJSON EquationTable where
  toJSON (EquationTable v) = toJSON v

instance FromJSON EquationTable where
  parseJSON (A.Array eqnA) = do
    (eqns :: Vector Equation) <- mapM A.parseJSON eqnA
    case mkEquationTable (toList eqns) of
      Nothing ->
        fail $
          mconcat
            [ "Failed to construct EquationTable: "
            , "too many elements, or equations are incompatible."
            ]
      Just eqnT -> pure eqnT
  parseJSON _ = fail "Incorrect type to construct an EquationTable."

instance (DrawConstraints t b) => Draw EquationTable t where
  draw (EquationTable eqns) =
    -- must sort the equations, otherwise equivalent EquationTables are drawn
    -- differently.
    Diagram.vsep 0.3 (draw <$> Vector.toList eqns)

-- | A 'Vector' of 'DirectionalEquation' in the order they should be applied
-- left-to-right.
type Trades = [DirectionalEquation]

-- | Create an 'EquationTable' of size @n@ from a given list. Returns 'Nothing'
-- if provided more than 10 'Equation's or if there exist duplicate Equations.
mkEquationTable :: [Equation] -> Maybe EquationTable
mkEquationTable eqns = EquationTable <$> equations
 where
  numEqns = length eqns
  equations =
    if numEqns <= 10 && length (nub eqns) == numEqns
      then Just (Vector.fromList eqns)
      else Nothing

-- | Map over the 'Equation's of an 'EquationTable'.
mapEquationTable :: (Equation -> a) -> EquationTable -> Vector a
mapEquationTable f = fmap f . coerce

-- | Map over the an 'EquationTable' where each 'Equation' has been flattned
-- into its corresponding 'DirectionalEquation's.
mapDirectionally :: (DirectionalEquation -> a) -> EquationTable -> Vector a
mapDirectionally f eqnT =
  foldl'
    (\acc (splitEquation -> (l, r)) -> [f l, f r] <> acc)
    mempty
    (coerce @_ @(Vector Equation) eqnT)

-- | Get the 'Equation's in the 'EquationTable'.
unwrapEquationTable :: EquationTable -> Vector Equation
unwrapEquationTable = coerce

-- | Is the provided @eqn@ in the 'EquationTable'?
inTable :: (Equationlike eqn) => EquationTable -> eqn -> Bool
inTable (EquationTable eqns) = (`elem` eqns) . promoteEqn

-- | Given a value with a 'PebbleSet' and the bank, filter the satisfiable
-- 'DirectionalEquation's from the table.
filterTable
  :: (ToPebbleSet a, ToPebbleSet bank)
  => EquationTable
  -> bank
  -> a
  -> Vector DirectionalEquation
filterTable (EquationTable eqns) bank wallet =
  foldl'
    ( \acc (l, r) -> case (satisfies wallet bank l, satisfies wallet bank r) of
        (True, True) -> [l, r] <> acc
        (True, False) -> Vector.cons l acc
        (False, True) -> Vector.cons r acc
        _ -> acc
    )
    mempty
    (fmap splitEquation eqns)

-- | Split an 'Equation' into both of its 'DirectionalEquation's.
splitEquation :: Equation -> (DirectionalEquation, DirectionalEquation)
splitEquation (Equation dirEqn) = (dirEqn, flipDirEqn dirEqn)

-- | Determine if the bank and the given value satisfies any 'Equation' in the
-- 'EquationTable'.
anySatisfies
  :: (ToPebbleSet a, ToPebbleSet bank) => EquationTable -> bank -> a -> Bool
anySatisfies eqnT bank wallet = not $ Vector.null eqns
 where
  eqns = filterTable eqnT bank wallet

-- | Given a source of randomness, generate a random 'Equation'.
mkRandomEquation :: forall g. (RandomGen g) => g -> (Equation, g)
mkRandomEquation g0 =
  let (ix, g') = Random.uniformR (0, 179) g0
   in eqns g' ix
 where
  eqns :: g -> Int -> (Equation, g) -- length = 180
  eqns g ix = go g (allPossibleEquations !! ix)
  go g' (lhs, rhs) =
    let (lhsMul, g'') = genSideMultiplicity g' lhs
        (rhsMul, g''') = genSideMultiplicity g'' rhs
        lhsMS = toPebbleSet $ MS.fromOccurList lhsMul
        rhsMS = toPebbleSet $ MS.fromOccurList rhsMul
        eqn = promoteEqn $ DirEqn lhsMS rhsMS
     in (eqn, g''')

-- | Internal function. List containing both sides of a potential 'Equation',
-- /without/ multiplicity applied to each element. The sides of the equations
-- do not have duplicate elements.
--
-- As of 0.1.0.0, there are 180 elements
-- @since 0.1.0.0
allPossibleEquations :: [([Pebble], [Pebble])]
allPossibleEquations = do
  lPeb <- allCombinations pebbles
  guard (length lPeb <= pebbleBound && lPeb /= [])
  rPeb <- allCombinations pebbles
  guard (length rPeb <= pebbleBound && rPeb /= [])
  guard (not $ hasAny lPeb rPeb)
  pure (lPeb, rPeb)
 where
  pebbleBound = fromEnum (maxBound @Pebble)

-- | Generate a side multiplicity for a bounded enumeration. The maximum value
-- for any single element is the max bound of the enumeration minus the number
-- of elements in the provided list.
--
-- Note: this function will perform poorly if the provided list is large, as it
--       requires indexing into the list to apply the multiplicity, which is slow.
genSideMultiplicity
  :: forall a g
   . (RandomGen g, Bounded a, Enum a)
  => g
  -> [a]
  -- ^ The elements for which to generate a side multiplicity.
  -> ([(a, Int)], g)
  -- ^ ([element, multiplicity], updated generator)
genSideMultiplicity g0 as =
  first (zip as)
    . go g' selectedMultiplicity
    $ fmap (const 1) as
 where
  (selectedMultiplicity, g') = Random.uniformR (0, maxMultiplicity) g0
  maxMultiplicity = fromEnum (maxBound @a) - sideColors
  -- distribute N randomly across the indices of the list, returning the list and
  -- a new generator.
  go g 0 ns = (ns, g)
  go g numLeft ns =
    let (ix, g') = Random.uniformR (0, sideColors - 1) g
     in go g' (numLeft - 1) $ setAt ns ix ((ns !! ix) + 1)
  sideColors = length as

  setAt :: forall a. [a] -> Int -> a -> [a]
  setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

-- | Create an 'EquationTable' given a random generator. The returned EquationTable
-- will not have duplicate elements. The generated 'EquationTable' will always have
-- 10 elements.
mkRandomEquationTable :: (RandomGen g) => g -> (EquationTable, g)
mkRandomEquationTable = go Nothing
 where
  -- retry in case of duplicates with a split generator
  go Nothing g' =
    let
      (g'', _) = Random.split g'
      eqnT =
        EquationTable
          . Sized.SomeSized
          $ Sized.unfoldrN @10 mkRandomEquation g''
      mbEqnT = if noDup eqnT then Just eqnT else Nothing
     in
      go mbEqnT g''
  go (Just eqnT) g' = (eqnT, g')
  noDup :: EquationTable -> Bool
  noDup = (== 10) . length . nub . Vector.toList . (coerce @_ @(Vector Equation))
