-- | Provides a VASS representation for 'DirectionalEquation's. A VASS, or
-- Vector Addition System with States, is a finite automaton isomorphic to
-- a single 'DirectionalEquation'. The graph traversal is performed with
-- Karp-Miller Trees, which--while lacking optimizations other means of
-- coverability checking provide--are plenty fast for /Bazaar/, and enable
-- storing the VASS's run.
--
-- The implementation is based on the work covered in
-- @<https://wrap.warwick.ac.uk/id/eprint/177504/1/WRAP_Theses_Dixon_2022.pdf>@.
-- It is recommended to read Chapter 3 (Preliminaries) and Chapter 4 (The
-- Coverability Problem and @HCover@).
--
-- Due to the employment of Karp-Miller Trees in coverability checking, we require
-- an /initial vector/ \(v_0\) and /target vector/ \(v\). \(v_0\) will often be a
-- player's wallet, while \(v\) is determined by the required wallet needed to
-- maximize the desired goal. Not every \(v\) can be satisfied, meaning an
-- optimizing strategy ought to have a list of /target vectors/ in order of
-- preference, failing only if none of the targets can be satisfied.
--
-- Note: in order to minimize number of graph traversals, the bank is included
-- alongside the 'DirectionalEquation' in order to ensure the bank is also
-- satisfied by the set of transitions. This comes at the downside of doubling
-- the dimensions of the vector, but it has very little perceived impact on
-- performance, given the depth limit of 4.
module Bazaar.Common.Equations.VASS where

import Bazaar.Common.Equations
import Bazaar.Common.Internal.Prelude
import Bazaar.Common.Pebbles
import Data.Map qualified as Map
import Data.MultiSet qualified as MS
import Data.VASS
import Data.VASS.Coverability
import Data.Vector qualified as Vector

-- | Internal name used for states of the graph. The graph we construct does
-- not require multiple labeled states.
vassState :: Name a
vassState = "μ"

-- | The number of dimensions used by our 'Bazaar' game. Includes a 'Hand' and
-- a 'Bank'.
dimensionality :: Integer
dimensionality = fromIntegral $ length (enumFrom @Pebble minBound) * 2

-- * Vector representation

--
-- The Vectors are represented as:
--
-- @
--      b  g  r w y   -- wallet
--     bb bg br bw by -- bank
-- @
--
-- left to right, top to bottom.

-- | Convert from the representation used for /Bazaar/ to the representation
-- used for VASSs. Note this only creates a vector with @dimensionality / 2@
-- elements. It is expected the other half be created similarly and concatenated
-- to create the final representation.
createVector :: (ToPebbleSet a) => a -> Vector Integer
createVector v =
  fmap
    (\peb -> fromIntegral $ MS.occur peb msPeb)
    (Vector.enumFromTo minBound maxBound)
 where
  msPeb = convert @(MultiSet Pebble) v

-- | Create a 'Transition' from a 'DirectionalEquation'. Transitions
-- are represented as tuples of pre- and post-transition requirements, each with
-- 'dimensionality' number of elements.
-- Example for @2G 1R 1Y = 3W@:
--
-- @
--             b g r w y    b g r w y
--     pre  = [0,2,1,0,1,   0,0,0,3,0] <- subtracted from wallet and bank
--     post = [0,0,0,3,0,   0,2,1,0,1] <- added to wallet and bank
--             ^-------^    ^-------^
--               hand          bank
-- @
--
-- Note that values in the @Transition@ cannot be negative. Please see the thesis
-- regarding the caveats with this restriction (the non-negative invariant
-- doesn't always apply).
--
-- The name created by this function is empty. It ought to be set before being
-- used to construct a 'VASS'.
createTransition :: DirectionalEquation -> Transition
createTransition dirEqn =
  Transition
    { name = ""
    , pre = costVec <> profitVec
    , post = profitVec <> costVec
    , nextState = vassState
    }
 where
  lhs = convert @(MultiSet Pebble) $ lhsToPebbleSet dirEqn
  rhs = convert @(MultiSet Pebble) $ rhsToPebbleSet dirEqn
  costVec = createVector lhs
  profitVec = createVector rhs

-- | Create a table of 'Transition's from an 'EquationTable'. The 'Map' only
-- has a single key, as we do not require more than one state. The @Map@
-- representation is purely to make construction of a 'VASS' more convenient.
createTransitionTable :: EquationTable -> Map (Name State) (Vector Transition)
createTransitionTable eqnT =
  Map.singleton vassState transitions
 where
  transitions = addTransitionNames $ mapDirectionally createTransition eqnT
  setName n x = x {name = n}
  addTransitionNames :: Vector Transition -> Vector Transition
  addTransitionNames ts =
    Vector.zipWith
      ($)
      ( Vector.fromList
          [setName $ coerce $ "t_" ++ show n | n <- [0 .. length ts - 1]]
      )
      ts

-- | Create a 'Conf' to be used as the intial state, which must include the
-- game's @Bank@.
createInitialConf :: (ToPebbleSet a, ToPebbleSet bank) => a -> bank -> Conf
createInitialConf a bank =
  Configuration
    { state = vassState
    , vec = createVector a <> createVector bank
    }

-- | Create a 'Conf' used as a target, which requires the bank simply not be
-- empty and the 'Hand' satisfy the provided target.
createTargetConf :: (ToPebbleSet a) => a -> Conf
createTargetConf a =
  Configuration
    { state = vassState
    , vec = createVector a <> createVector (MS.empty @Pebble)
    }

-- | Convert an 'EquationTable' into its 'VASS' representation. This operation
-- is one-way, as a @VASS@ cannot safely be converted to an @EquationTable@.
createVASS :: EquationTable -> VASS
createVASS eqnT =
  VASS
    { dimension = dimensionality
    , places = fmap (coerce . name) transitionVec
    , states = [vassState]
    , transitions = transitionMap
    }
 where
  transitionMap = createTransitionTable eqnT
  transitionVec = transitionMap Map.! vassState

-- | The main entry point to the coverability checking. Creates the coverability
-- problem ('CovProblem'), which must be checked for safety (i.e., satisfiability).
createCovProblem
  :: (ToPebbleSet init, ToPebbleSet bank, ToPebbleSet target)
  => EquationTable
  -> (init, bank)
  -> target
  -> CovProblem
createCovProblem eqnT (init, bank) target =
  CovProblem
    (createVASS eqnT)
    (createInitialConf init bank)
    (createTargetConf target)

-- | The main entry point to the coverability checking. Creates the coverability
-- problem ('CovProblem'), which must be checked for safety (i.e., satisfiability).
--
-- This is more efficient if you plan to run multiple 'CovProblem's on the same
-- 'EquationTable' because it does not needlessly convert the same 'EquationTable'
-- to a 'VASS' every time.
createCovProblem'
  :: (ToPebbleSet init, ToPebbleSet bank, ToPebbleSet target)
  => VASS
  -> (init, bank)
  -> target
  -> CovProblem
createCovProblem' vass (init, bank) target =
  CovProblem
    vass
    (createInitialConf init bank)
    (createTargetConf target)
