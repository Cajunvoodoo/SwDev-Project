-- | See documentation for "Bazaar.Player.KMStrategy".
module Bazaar.Player.CardMaximizer where

import Bazaar.Common
import Bazaar.Common.Equations.KarpMiller
import Bazaar.Common.RuleBook
import Bazaar.Player.KMStrategy
import Bazaar.Player.Strategy
import Data.List (sort, sortOn)
import Data.MultiSet qualified as MS
import Data.Ord (Down (..))
import GHC.Float (int2Double)

maxCardBuyKMStrategy :: KMStrategy
maxCardBuyKMStrategy =
  KMStrategy
    { takePebbleStrat = maxCardBuyPebbleStrategy
    , takeEquationStrat = maxCardBuyEquationStrategy
    }

maxCardBuyPebbleStrategy :: KMPebbleStrategy
maxCardBuyPebbleStrategy = KMPebbleStrategy {..}
 where
  shouldTakePebble :: PureRules -> Hand -> Bank -> [CardBuy] -> Bool
  shouldTakePebble _ _ [] _ = False
  shouldTakePebble pr hand bank cbs = evPebble >= evNoPebble
   where
    -- expected /number of cards/ bought if no pebble is drawn
    evNoPebble :: Double
    evNoPebble = l2d $ decidedPebbleCardBuy pr hand cbs
    -- expected /number of cards/ bought if a random pebble drawn
    evPebble :: Double
    evPebble = sum evs / l2d evs
     where
      evs :: [Double]
      evs = for pebbles \peb ->
        let cbLen = l2d $ needPebbleCardBuy pr hand cbs peb
            bankSize = MS.size (convert @(MultiSet Pebble) bank)
            pebProb
              | bankSize == 0 = 0
              | otherwise =
                  int2Double (MS.occur peb (convert bank)) / int2Double bankSize
         in cbLen * pebProb

  needPebbleCardBuy :: PureRules -> Hand -> [CardBuy] -> (Pebble -> CardBuy)
  needPebbleCardBuy pr hand cbs peb =
    decidedPebbleCardBuy pr (convert $ hand `union` peb) cbs

  decidedPebbleCardBuy :: PureRules -> Hand -> [CardBuy] -> CardBuy
  decidedPebbleCardBuy pr hand (filter (`isSubset` hand) -> cbs) =
    tiebreakCardBuys pr hand maxCardBuys
   where
    maxNumCards :: Int
    maxNumCards = length $ maximumBy (compare `on` length) cbs

    maxCardBuys :: [CardBuy]
    maxCardBuys = filter ((== maxNumCards) . length) cbs

maxCardBuyEquationStrategy :: KMEquationStrategy
maxCardBuyEquationStrategy =
  KMEquationStrategy
    { cardHeuristic = maxCardBuyHeuristic
    , maximizeCovRes = maxCardBuyMaximization
    }

maxCardBuyHeuristic :: [CardBuy] -> [CardBuy]
maxCardBuyHeuristic =
  nubOrd . fmap sort . filter (not . null) . sortOn (Down . length)

maxCardBuyMaximization
  :: PureRules -> [CoverabilityResult] -> (Trades, CardBuy)
maxCardBuyMaximization pr crs = do
  tiebreakCovResults pr maxCardBuys
 where
  -- assuming the result is non-empty, the heuristic places the largest number
  -- of card buys at the front
  maxNumCards :: Int
  maxNumCards =
    case crs of
      [] -> 0 -- our heuristic removes null lists, but we still should check
      (c : _) -> length $ cardBuy c

  maxCardBuys :: [CoverabilityResult]
  maxCardBuys = takeWhile ((== maxNumCards) . length . cardBuy) crs
