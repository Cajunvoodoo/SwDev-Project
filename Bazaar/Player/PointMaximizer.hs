-- | See documentation for "Bazaar.Player.KMStrategy".
module Bazaar.Player.PointMaximizer where

import Bazaar.Common
import Bazaar.Common.Equations.KarpMiller
import Bazaar.Common.RuleBook
import Bazaar.Player.KMStrategy
import Bazaar.Player.Strategy
import Data.List (sort, sortOn)
import Data.MultiSet qualified as MS
import Data.Ord (Down (..))
import GHC.Float (int2Double)

maxPointBuyKMStrategy :: KMStrategy
maxPointBuyKMStrategy =
  KMStrategy
    { takePebbleStrat = maxPointBuyPebbleStrategy
    , takeEquationStrat = maxPointBuyEquationStrategy
    }

maxPointBuyPebbleStrategy :: KMPebbleStrategy
maxPointBuyPebbleStrategy = KMPebbleStrategy {..}
 where
  shouldTakePebble :: PureRules -> Hand -> Bank -> [CardBuy] -> Bool
  shouldTakePebble _ _ [] _ = False
  shouldTakePebble pr hand bank cbs = evPebble >= evNoPebble
   where
    calcPoints :: (ToPebbleSet a, FromPebbleSet a) => a -> CardBuy -> Double
    calcPoints h cb = fromIntegral . snd $ calculatePoints pr h cb
    -- expected /number of points/ if no pebble is drawn
    evNoPebble :: Double
    evNoPebble = calcPoints hand $ decidedPebbleCardBuy pr hand cbs
    -- expected /number of points/ if a random pebble drawn
    evPebble :: Double
    evPebble = sum evs / l2d evs
     where
      evs :: [Double]
      evs = for pebbles \peb ->
        let hand' = hand `union` peb
            pebPoints = calcPoints hand' $ needPebbleCardBuy pr hand cbs peb
            bankSize = MS.size (convert @(MultiSet Pebble) bank)
            pebProb
              | bankSize == 0 = 0
              | otherwise =
                  int2Double (MS.occur peb (convert bank)) / int2Double bankSize
         in pebPoints * pebProb

  needPebbleCardBuy :: PureRules -> Hand -> [CardBuy] -> Pebble -> CardBuy
  needPebbleCardBuy pr hand cbs (MS.singleton -> peb) =
    decidedPebbleCardBuy pr (convert $ hand `union` peb) cbs

  decidedPebbleCardBuy :: PureRules -> Hand -> [CardBuy] -> CardBuy
  decidedPebbleCardBuy pr hand (filter (`isSubset` hand) -> cbs) =
    tiebreakCardBuys pr hand cbs

maxPointBuyEquationStrategy :: KMEquationStrategy
maxPointBuyEquationStrategy =
  KMEquationStrategy
    { cardHeuristic = maxPointBuyHeuristic
    , maximizeCovRes = maxPointBuyMaximization
    }

maxPointBuyHeuristic :: [CardBuy] -> [CardBuy]
maxPointBuyHeuristic =
  nubOrd . fmap sort . filter (not . null) . sortOn (Down . length)

maxPointBuyMaximization
  :: PureRules -> [CoverabilityResult] -> (Trades, CardBuy)
maxPointBuyMaximization _ [] = ([], [])
maxPointBuyMaximization pr crs = (ts, cb)
 where
  maxPoints = maximum (fmap (maximum . calcPointsCovRes pr) crs)

  maxPointsCrs = filterCovRes (\(h, _) cb -> maxPoints == snd (calculatePoints pr h cb)) crs

  (ts, cb) = tiebreakCovResults pr maxPointsCrs
