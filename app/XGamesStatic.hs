module Main where

import Bazaar.Common hiding (toList)
import Bazaar.Common.RuleBook
import Bazaar.Player.Mechanism
import Bazaar.Player.Player
import Data.Default
import Bazaar.Player.Reachability
import Bazaar.Referee.Observer
import Bazaar.Referee.Referee
import Bazaar.State.GameState
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.IORef
import Data.List qualified as List
import Effectful
import GHC.IsList (IsList (..))
import System.Directory
import XGamesCommon
import Effectful.Ki (runStructuredConcurrency)
import Effectful.Concurrent (runConcurrent)

main :: IO ()
main = do
  tmpExists <- doesDirectoryExist "Tmp"
  when tmpExists (removeDirectoryRecursive "Tmp")
  createDirectoryIfMissing False "Tmp"
  ioref <- newIORef mempty
  (winnerNames, kickedNames) <- xgames ioref
  BS.putStrLn $ encode winnerNames
  BS.putStrLn $ encode kickedNames

-- | Run the testing task for milestone 3. Stream-parses JSON from stdin,
-- taking an EquationTable, a 'Hand', and a
xgames :: IORef (Vector (GameState Pure, Int)) -> IO ([Text], [Text])
xgames ioref = do
  streamParseJson 3 BS.getLine isEOF >>= \case
    Left fail ->
      error $ "failed to create 3-tuple of equations: " <> fail
    Right [eqnsV, exchangesV, tsV] -> do
      let actorsRes = fromJSON @[Actor] eqnsV
          eqnsRes = fromJSON @EquationTable exchangesV
          gsRes = fromJSON @(GameState Void) tsV
      case (,,) <$> actorsRes <*> eqnsRes <*> gsRes of
        Success (actors, eqnT, gsNoEqns) -> do
          let playerNames =
                fromList $
                  setName
                    <$> zip
                      (fmap getName actors)
                      (toList $ players gsNoEqns)
              gs = gsNoEqns {equations = eqnT, players = playerNames}
              playersStrat = fromList (actorToRemotePlayerWithStrat <$> actors)
          (fmap getName -> winners, fmap getName -> kicked) <-
            runEff
              . runConcurrent
              . runStructuredConcurrency
              . runRules 4 20
              . runPurePlayerInteraction eqnT playersStrat
              . addExnsPlayerInteraction (mapActorsToExnFunc actors)
              . addCheatingPlayerInteraction eqnT actors
              . addLoopingPlayerInteraction def actors
              . runObserverInteractionIO ioref
              . runObserverInteractionFile
              -- . recordGameStates
              $ refereeInit gs
          pure (List.sort winners, List.sort kicked)
        Error err -> fail err
    _ -> fail "Invalid 3-tuple"
 where
  setName (name, rp) = rp {conn = PureComm name}
