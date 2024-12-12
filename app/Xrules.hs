module Main where

import Bazaar.Common
import Bazaar.Common.RuleBook
import Bazaar.Player.Player
import Bazaar.Player.Reachability
import Bazaar.State.GameState
import Bazaar.State.TurnState
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BS
import Effectful
import Effectful.Error.Static qualified as Error

main :: IO ()
main =
  xrule >>= \case
    Nothing -> BS.putStrLn $ A.encode False
    Just (hand, bank) -> do
      BS.putStrLn $ A.encode hand
      BS.putStrLn $ A.encode bank

-- | Run the testing task for milestone 3. Stream-parses JSON from stdin,
-- taking an EquationTable, a 'Hand', and a
xrule :: IO (Maybe (Hand, Bank))
xrule = do
  streamParseJson 3 BS.getLine isEOF >>= \case
    Left fail ->
      error $ "failed to create 3-tuple of equations: " <> fail
    Right [eqnsV, exchangesV, tsV] -> do
      let eqnsRes = A.fromJSON @EquationTable eqnsV
          exchangesRes = A.fromJSON @[DirectionalEquation] exchangesV
          tsRes = A.fromJSON @(TurnState Pure) tsV
      case (,,) <$> eqnsRes <*> exchangesRes <*> tsRes of
        A.Success (eqnT, exchanges, ts) -> do
          let gs = mkGameState eqnT ts
              runRules = runRules 4 20 gs
              verifyExchange = do
                ts'@TurnState {bank} <-
                  performPTAction eqnT ts (Exchange exchanges)
                let PlayerState {hand} = getPlayerState ts'
                pure (Just (hand, bank))
              run =
                runPureEff
                  . Error.runErrorNoCallStackWith (\_ -> pure Nothing)
                  $ runRules verifyExchange
          pure run
        A.Error err -> fail err
    _ -> fail "Invalid 3-tuple"

mkGameState :: EquationTable -> TurnState p -> GameState p
mkGameState equations TurnState {..} =
  GameState
    { cardPile = mempty
    , players = [activePlayer]
    , ..
    }
