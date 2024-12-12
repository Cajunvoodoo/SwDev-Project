{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}

module Bazaar.Player.KMStrategy where

import Bazaar.Common
import Bazaar.Common.Equations.KarpMiller
import Bazaar.Common.RuleBook
import Bazaar.Player.Player
import Bazaar.Player.Strategy
import Bazaar.State.TurnState
import Data.List (foldl1')
import GHC.Float (int2Double)

-- | Strategy governing what ought to happen when no 'Equation's can be applied
-- and a Player opts to draw a 'Pebble'.
data KMPebbleStrategy = KMPebbleStrategy
  { shouldTakePebble :: PureRules -> Hand -> Bank -> [CardBuy] -> Bool
  -- ^ When 'Equation's cannot be taken, should a 'Pebble' be randomly picked, or
  -- should no 'Pebble' be taken?
  , needPebbleCardBuy :: PureRules -> Hand -> [CardBuy] -> (Pebble -> CardBuy)
  -- ^ Function used after deciding whether a 'Pebble' should be taken.
  -- Partially applied and returned as a continuation in the 'NeedPebble'
  -- case of 'KMStrategyResult'.
  , decidedPebbleCardBuy :: PureRules -> Hand -> [CardBuy] -> CardBuy
  -- ^ Used when 'shouldTakePebble' returns 'False'.
  }

-- | Strategy governing how a Player ought to make Exchanges when
-- 'Equations' can be applied.
data KMEquationStrategy = KMEquationStrategy
  { cardHeuristic :: [CardBuy] -> [CardBuy]
  -- ^ Heuristic sorting condition for the card permutations. The head of the
  -- list ought to be the most perferable result (e.g., sort by length to
  -- maximize number of cards bought).
  , maximizeCovRes :: PureRules -> [CoverabilityResult] -> (Trades, CardBuy)
  -- ^ Given a list of 'CoverabilityResult's, convert them into the
  -- 'DirectionalEquation's to apply and the 'CardBuy' to buy.
  }

-- | Karp-Miller-based Strategies. Run them with 'runStrategy'.
data KMStrategy = KMStrategy
  { takePebbleStrat :: KMPebbleStrategy
  , takeEquationStrat :: KMEquationStrategy
  }

instance Strategy KMStrategy where
  runStrategy
    pr
    KMStrategy
      { takePebbleStrat = KMPebbleStrategy {..}
      , takeEquationStrat = KMEquationStrategy {..}
      }
    eqnT
    ts@TurnState {activePlayer = RemotePlayer {playerState = PlayerState {..}}, ..}
      | canExchange =
          let covRes = runCoverability eqnT ts cardHeuristic
              (trades, cb) = maximizeCovRes pr covRes
              turnActions = WithExchangeTurnActions trades cb
           in Decided turnActions
      | shouldTakePebble pr hand bank cbs =
          NeedPebble $ WithExchangeTurnActions [] . needPebbleCardBuy pr hand cbs
      | otherwise =
          Decided . WithExchangeTurnActions [] $ decidedPebbleCardBuy pr hand cbs
     where
      cbs = permuteWithoutRep cardsInPlay
      canExchange = anySatisfies eqnT bank hand

-- | Pick the best Trade and CardBuy out of a list of 'CoverabilityResult's.
-- All of the provided 'CoverabilityResult's ought to be equivalent under the
-- primary strategy. E.g., maxizing card buys ought to pass a list where all
-- of the 'CardBuy's in the 'CoverabilityResult' have equal length.
tiebreakCovResults :: PureRules -> [CoverabilityResult] -> (Trades, CardBuy)
tiebreakCovResults _ [] = ([], [])
tiebreakCovResults pr cvs =
  let (_, ts, cb) =
        foldl1' (tiebreak pr) $ flattenCovRes cvs
   in (ts, cb)

-- | Length-to-Double. Converts the length of the list to a double.
l2d :: [a] -> Double
l2d = int2Double . length
