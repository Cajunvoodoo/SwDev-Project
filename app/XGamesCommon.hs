module XGamesCommon where

import Bazaar.Common
import Bazaar.Common.RuleBook
import Bazaar.Player.CardMaximizer
import Bazaar.Player.KMStrategy
import Bazaar.Player.Mechanism
import Bazaar.Player.Player
import Bazaar.Player.PointMaximizer
import Bazaar.Player.Reachability
import Bazaar.Player.Strategy
import Bazaar.State.TurnState
import Data.Map qualified as Map
import Data.Aeson
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared qualified as State
import GHC.IsList (IsList (..))
import System.Random qualified as Random
import Text.RE.TDFA.Text
import Effectful.Concurrent.STM
import Effectful.Ki (StructuredConcurrency)
import Effectful.State.Static.Shared (State)
import qualified Data.Maybe as Maybe

actorToRemotePlayerWithStrat :: Actor -> (RemotePlayer Pure, SomeStrategy)
actorToRemotePlayerWithStrat = \case
  NoExnActor name pp -> mkTup name pp
  WithExnActor name pp _ -> mkTup name pp
  WithCheatActor name pp _ -> mkTup name pp
  WithLoopActor name pp _ _ -> mkTup name pp
 where
  mkTup name pp = (mkRP name, SomeStrategy $ policyToStrategy pp)
  mkRP name = RemotePlayer {conn = PureComm name, playerState = emptyPS}
  emptyPS =
    PlayerState
      { points = 0
      , hand = mempty
      , cards = []
      }

-- | Functions for which Actors can throw exceptions.
-- | Functions for which Actors can throw exceptions.
-- | Convert a list of Actors into the function used by 'addExnsPlayerInteraction'.
mapActorsToExnFunc
  :: [Actor] -> (forall m b. (Monad m) => Text -> PlayerInteraction p m b -> m ())
mapActorsToExnFunc = go (\_ _ -> pure ())
 where
  go
    :: (forall m b. (Monad m) => Text -> PlayerInteraction p m b -> m ())
    -> [Actor]
    -> (forall m b. (Monad m) => Text -> PlayerInteraction p m b -> m ())
  go f [] = f
  go f (WithExnActor name _ exn : as) =
    go
      ( \name' exn' -> do
          ( if name' == name && piMatchesExn exn exn'
              then mkExn name (show exn)
              else f name' exn'
            )
      )
      as
  go f (_ : as) = go f as
  mkExn name func =
    error $
      "Player " <> show name <> " threw an exception in function " <> func

-- | Does the 'ActorExn' match the 'PlayerInteraction'?
piMatchesExn :: ActorExn -> PlayerInteraction p m b -> Bool
piMatchesExn = \cases
  ExnSetup (SetupPlayer _ _) -> True
  ExnAskPebbleEqn (AskPebbleOrTrades _) -> True
  ExnAskForCards (AskForCardBuy _) -> True
  ExnWin (Win _ _) -> True
  _ _ -> False
