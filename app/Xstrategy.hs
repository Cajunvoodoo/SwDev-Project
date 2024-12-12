{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Bazaar.Common
import Bazaar.Player.CardMaximizer
import Bazaar.Player.KMStrategy
import Bazaar.Player.Player
import Bazaar.Player.PointMaximizer
import Bazaar.Player.Reachability
import Bazaar.Player.Strategy
  ( Strategy (runStrategy)
  , StrategyResult (..)
  , TurnActions (..)
  )
import Bazaar.State.TurnState
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BS
import Bazaar.Common.RuleBook
import Bazaar.State.GameState
import Effectful

main :: IO ()
main = do
  (trades, cb, pointsGained, hand) <- xstrategy
  let pEncode = BS.putStrLn . A.encode
  pEncode trades
  pEncode cb
  pEncode pointsGained
  pEncode hand

-- | Run the testing task for milestone 3. Stream-parses JSON from stdin,
-- taking an EquationTable, a 'Hand', and a :3
xstrategy :: IO ([DirectionalEquation], CardBuy, Natural, Hand)
xstrategy = do
  streamParseJson 3 BS.getLine isEOF >>= \case
    Left fail ->
      error $ "failed to create 3-tuple of equations: " <> fail
    Right [eqnsV, turnState, policy] -> do
      let eqnTRes = A.fromJSON @EquationTable eqnsV
          turnStateRes = A.fromJSON @(TurnState Pure) turnState
          policyRes = A.fromJSON @PurchasePolicy policy
      case (,,) <$> eqnTRes <*> turnStateRes <*> policyRes of
        A.Success (eqnT, ts@TurnState {..}, policy) ->
          let gs = mkGameState eqnT ts
          in runEff $ runRules 4 20 do
          pr <- getPureRules
          let strat = policyToStrategy policy
              stratRes = runStrategy pr strat eqnT ts
          case stratRes of
            NeedPebble _ -> error "provided cases ought to allow exchanges of equations"
            Decided actions ->  do
              let player = playerState activePlayer
                  exchanges = taExchanges actions
                  cardBuys = taCardBuy actions
                  hand' = hand player
              eqnsRes <- evalEqns (hand', bank) exchanges
              let (Just (hand'', bank')) = eqnsRes
              let player' = player {hand = hand''}
              let (Just (playerFinal, _bankFinal)) = evalBuys pr (player', bank') cardBuys
              let pointsGained = points playerFinal - points player
              pure (exchanges, cardBuys, pointsGained, hand playerFinal)
        A.Error err -> fail err
    _ -> fail "Invalid 3-tuple"

policyToStrategy :: PurchasePolicy -> KMStrategy Pure
policyToStrategy = \case
  PurchasePoints -> maxPointBuyKMStrategy
  PurchaseSize -> maxCardBuyKMStrategy

data PurchasePolicy
  = PurchasePoints
  | PurchaseSize
  deriving (Eq, Show)

instance FromJSON PurchasePolicy where
  parseJSON = A.withText "PurchasePolicy" \case
    "purchase-points" -> pure PurchasePoints
    "purchase-size" -> pure PurchaseSize
    s -> fail $ "invalid string for PurchasePolicy: " <> show s

mkGameState :: EquationTable -> TurnState p -> GameState p
mkGameState equations TurnState {..} =
  GameState
    { cardPile = mempty
    , players = [activePlayer]
    , ..
    }
