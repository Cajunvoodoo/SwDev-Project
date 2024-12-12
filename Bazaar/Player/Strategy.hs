{-# LANGUAGE UndecidableInstances #-}

module Bazaar.Player.Strategy where

import Bazaar.Common
import Bazaar.Common.RuleBook hiding (Exchange)
import Bazaar.State.TurnState
import Data.List (sort)
import Data.MultiSet qualified as MS

-- | Actions that can be performed by an instance of 'Strategy'.
--
-- NOTE: The exclusion of a "DrawPebble" is explcit; the continuation in
-- 'StrategyResult' would indicate the need for a Client to communicate with the
-- referee that it wants to draw a pebble, from which the result can be passed
-- back to complete the 'TurnAction' creation process.
data TurnActions
  = WithExchangeTurnActions
      { taExchanges :: [DirectionalEquation]
      , taCardBuy :: CardBuy
      }
  deriving stock (Show, Eq)

-- | The result of running a 'Strategy'.
data StrategyResult
  = NeedPebble (Pebble -> TurnActions)
  | Decided TurnActions

instance Show StrategyResult where
  show (NeedPebble _) = "NeedPebble"
  show (Decided ta) = "Decided: " <> show ta

instance Eq StrategyResult where
  (==) :: StrategyResult -> StrategyResult -> Bool
  (==) (Decided lTAs) (Decided rTAs) = lTAs == rTAs
  (==) (NeedPebble lK) (NeedPebble rK) =
    and $ for pebbles \peb -> lK peb == rK peb
  (==) _ _ = False

-- | The governing behavior behind the decision making of a Player in a single
-- turn.
class Strategy a where
  -- Run a given strategy.
  runStrategy :: PureRules -> a -> EquationTable -> TurnState p -> StrategyResult

-- | Apply the tiebreaking rules to the provided tuple of 'Hand', 'Trades', and
-- 'CardBuy's. Only the *final* results should be provided to 'tiebreak', otherwise
-- an erroneous result will be chosen (e.g., folding with the empty trade).
-- The 'Hand' is returned for ease-of-use with folding. Thus, it is likely one
-- would prefer to use 'tiebreakCovResults' or 'tiebreakCardBuys' instead, as
-- these functions do not make this error.
tiebreak
  :: PureRules
  -> (Hand, Trades, CardBuy)
  -> (Hand, Trades, CardBuy)
  -> (Hand, Trades, CardBuy)
tiebreak pr (lH, lTrades, sort -> lCB) (rH, rTrades, sort -> rCB)
  | length lTrades < length rTrades = l
  | length lTrades > length rTrades = r
  | otherwise = tiebreakPoints
 where
  l = (lH, lTrades, lCB)
  r = (rH, rTrades, rCB)
  (lPostBuy, lPoints) = calculatePoints pr lH lCB
  (rPostBuy, rPoints) = calculatePoints pr rH rCB
  lPostBuyLen = MS.size $ convert @(MultiSet Pebble) lPostBuy
  rPostBuyLen = MS.size $ convert @(MultiSet Pebble) rPostBuy
  tiebreakPoints
    | lPoints > rPoints = l
    | lPoints < rPoints = r
    | otherwise = tiebreakPebbleRem
  tiebreakPebbleRem
    | lPostBuyLen > rPostBuyLen = l
    | lPostBuyLen < rPostBuyLen = r
    | otherwise = tiebreakComparison
  tiebreakComparison
    | lPostBuy < rPostBuy = l
    | lPostBuy > rPostBuy = r
    | otherwise = tiebreakCardSeq
  tiebreakCardSeq
    | lCB < rCB = l
    | lCB > rCB = r
    | otherwise = tiebreakExchanges
  tiebreakExchanges
    | lTrades < rTrades = l
    | lTrades > rTrades = r
    | otherwise = l

-- | Pick the best Trade and CardBuy from a given 'Hand' and a list of 'CardBuy'.
-- All of the provided 'CoverabilityResult's ought to be equivalent under the
-- primary strategy. E.g., maxizing card buys ought to pass a list where all
-- of the 'CardBuy's in the 'CoverabilityResult' have equal length.
tiebreakCardBuys :: PureRules -> Hand -> [CardBuy] -> CardBuy
tiebreakCardBuys pr hand =
  foldl'
    ( \accCb cb -> do
        let (_, _, cb') = tiebreak pr (hand, [], accCb) (hand, [], cb)
         in cb'
    )
    []

-- | Simple existential type for 'Strategy' implementations.
data SomeStrategy where
  SomeStrategy :: (Strategy s) => s -> SomeStrategy

instance Show SomeStrategy where
  show _ = "SomeStrategy"

instance Strategy SomeStrategy where
  runStrategy pr (SomeStrategy s) = runStrategy pr s
