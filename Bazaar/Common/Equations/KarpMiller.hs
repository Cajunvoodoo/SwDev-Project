module Bazaar.Common.Equations.KarpMiller where

import Bazaar.Common
import Bazaar.Common.Equations.VASS
import Bazaar.Common.RuleBook
import Bazaar.Player.Player
import Bazaar.Player.Reachability
import Bazaar.State.TurnState
import Data.Bifunctor (bimap)
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.MultiSet qualified as MS
import Data.Ord (Down (..))
import Data.VASS
import Data.VASS.Coverability
import Data.VASS.Coverability.KarpMiller
import Data.VASS.Coverability.KarpMiller.ExtendedNaturals
import Data.Vector qualified as Vector
import System.Random qualified as Random

---------------------------------------------------------------------------------
-- Coverability Checking
---------------------------------------------------------------------------------

-- | The result of running a coverability test. Created most often through the
-- use of 'runCoverability'.
data CoverabilityResult = CovRes
  { runs :: [(PebbleSet, Trades)]
  -- ^ All of the resulting 'PebbleSet's and Exchanges needed to acquire
  -- 'cardBuy'.
  , cardBuy :: CardBuy
  -- ^ The cards to acquire. Bought using the 'PebbleSet's in 'runs'.
  }
  deriving stock (Show)

-- | Maximum number of 'Equation's a strategy can apply in a single turn.
maxEquationApplication :: Natural
maxEquationApplication = 4

-- | Run a coverage problem on the given 'EquationTable' and 'TurnState'.
-- The resulting list is sorted by the provided heuristic *before* constructing
-- a Karp-Miller Tree.
runCoverability
  :: EquationTable
  -> TurnState p
  -> ([CardBuy] -> [CardBuy])
  -- ^ Heuristic sorting condition for the card permutations. The head of the
  -- list ought to be the most perferable result (e.g., sort by length to
  -- maximize number of cards bought).
  -> [CoverabilityResult]
  -- ^ List of the results for each 'CardBuy'.
runCoverability
  eqnT
  TurnState {activePlayer = RemotePlayer {playerState = PlayerState {..}}, ..}
  heuristicSort = convertedKM
   where
    -- list of every possible ordering in which you can buy cards.
    -- this is threaded through the following computations in order to keep track
    -- of the results associated with each card buy.
    cardBuys :: [CardBuy]
    cardBuys = heuristicSort $ permuteWithoutRep cardsInPlay

    -- total cost of a given purchase, and the cards of that purchase
    cardBuyCosts :: [(PebbleSet, CardBuy)]
    cardBuyCosts = zip (fmap toPebbleSet cardBuys) cardBuys

    vass@VASS {transitions} = createVASS eqnT

    -- lookup table to convert state transitions to equations
    -- NOTE: :FailureOnVASSLookups
    -- Map.! is used despite safety concerns. If our state which we just created
    --   is not in the map, something is *critically wrong* and there is no
    --   logical way to continue (and failing with empty list is misleading).
    --   thus, we would rather fail fast than handle (effectively) corrupt data.
    dirEqnLookup =
      Vector.toList
        . Vector.zip (transitions Map.! vassState)
        $ mapDirectionally id eqnT

    -- helper for the lookup
    -- SEE: :FailureOnVASSLookups
    forceLookup a b = Maybe.fromJust $ lookup a b

    -- perform a lookup into 'dirEqnLookup'
    lookupDirEqns :: (Functor f) => f Transition -> f DirectionalEquation
    lookupDirEqns = fmap (`forceLookup` dirEqnLookup)

    -- Associate coverage problem with card buys
    covProblems :: [(CovProblem, CardBuy)]
    covProblems = first (createCovProblem' vass (hand, bank)) <$> cardBuyCosts

    -- convert VASS end state into a PebbleSet.
    -- Similar to :FailureOnVASSLookups, we fail if we encounter ω in the vectors.
    -- An ω indicates that value can be pumped, but no value should be pumpable
    -- in our trees, therefore encountering one is a major issue.
    extConfToPS :: ExtConf -> PebbleSet
    extConfToPS (Configuration {vec}) =
      toPebbleSet
        . MS.fromOccurList
        . Vector.toList
        . Vector.zip (Vector.enumFromTo (minBound @Pebble) maxBound)
        . Vector.take (fromInteger $ dimensionality `div` 2)
        $ fmap (fromInteger . fromFinite) vec

    -- Run karp-miller on each CovProblem
    karpMiller :: [((CovResult, [(ExtConf, Vector Transition)]), CardBuy)]
    karpMiller = first (karpMiller' (fromEnum maxEquationApplication)) <$> covProblems

    -- Filter out the unsafe (unsatisfiable) results
    karpMillerSafe :: [([(ExtConf, Vector Transition)], CardBuy)]
    karpMillerSafe = fmap (first snd) . filter ((== Safe) . fst . fst) $ karpMiller

    -- make a single CoverabilityResult, helper for 'convertedKM'
    mkCovRes :: ([(ExtConf, Vector Transition)], CardBuy) -> CoverabilityResult
    mkCovRes (vassRes, cardBuy) =
      CovRes
        { cardBuy = cardBuy
        , runs = bimap extConfToPS (lookupDirEqns . Vector.toList) <$> vassRes
        }

    -- interpret the karpMillerSafe results in a Bazaar-relevant form.
    convertedKM :: [CoverabilityResult]
    convertedKM = mkCovRes <$> karpMillerSafe

---------------------------------------------------------------------------------
-- CoverabilityResult Helpers
---------------------------------------------------------------------------------

-- | Calculate the points of each result of the provided 'CoverabilityResult'.
calcPointsCovRes :: PureRules -> CoverabilityResult -> [Natural]
calcPointsCovRes pr cr = points
 where
  points = fmap (\(h, _, cb) -> snd $ calculatePoints pr h cb) crFlat
  crFlat = flattenCovRes [cr]

-- | Flatten a 'CoverabilityResult'.
flattenCovRes :: [CoverabilityResult] -> [(Hand, Trades, CardBuy)]
flattenCovRes cvs =
  concatFor cvs \CovRes {..} ->
    for runs \(ps, ts) ->
      (convert ps, ts, cardBuy)

-- | Filter the runs of each 'CoverabilityResult' by the given predicate.
filterCovRes
  :: ((Hand, Trades) -> CardBuy -> Bool)
  -> [CoverabilityResult]
  -> [CoverabilityResult]
filterCovRes f cvs = filterAll
 where
  filterAll :: [CoverabilityResult]
  filterAll = fmap (\cr -> filterOne (decideInclusion cr) cr) cvs

  decideInclusion :: CoverabilityResult -> [Bool]
  decideInclusion CovRes {..} = fmap (\(h, ts) -> f (convert h, ts) cardBuy) runs

  filterOne :: [Bool] -> CoverabilityResult -> CoverabilityResult
  filterOne keep cr@CovRes {..} = cr {runs = fmap snd . filter fst $ zip keep runs}

---------------------------------------------------------------------------------
-- EXAMPLES
---------------------------------------------------------------------------------

exampleCov :: [CoverabilityResult]
exampleCov = runCoverability eqnT turnState (sortOn (Down . length))
 where
  turnState =
    TurnState
      { bank = convert $ mkListOfN 10000 mkRandomPebble (Random.mkStdGen 4)
      , cardsInPlay =
          [ Maybe.fromJust $
              mkCard ([RedPbl, BluePbl, BluePbl, BluePbl, BluePbl] :: [Pebble]) HappyFace
          , Maybe.fromJust $
              mkCard ([YellowPbl, BluePbl, BluePbl, BluePbl, BluePbl] :: [Pebble]) HappyFace
          ]
      , otherPlayerScores = [10, 6, 8, 5, 7]
      , activePlayer =
          RemotePlayer
            { conn = PureComm "NAME"
            , playerState =
                PlayerState
                  { hand =
                      convert
                        ([YellowPbl, RedPbl, RedPbl, BluePbl, BluePbl, BluePbl, BluePbl] :: [Pebble])
                  , points = 10
                  , cards = []
                  }
            }
      }
  eqnT = Maybe.fromJust $ mkEquationTable [promoteEqn eqn1] -- mkRandomEquationTable (Random.mkStdGen 10)
  eqn1 = Maybe.fromJust $ mkDirEquation [RedPbl] (replicate 4 BluePbl)
