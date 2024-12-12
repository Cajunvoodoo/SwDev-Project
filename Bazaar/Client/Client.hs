module Bazaar.Client.Client where

import Bazaar.Client.Referee
import Bazaar.Common
import Bazaar.Player.Player
import Bazaar.Player.Strategy
import Bazaar.State.TurnState
import Data.MultiSet qualified as MS
import Effectful
import Effectful.Error.Static (runErrorNoCallStackWith)
import Effectful.Network
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Bazaar.Player.Mechanism
import Bazaar.Common.RuleBook

-- | Run the client using 'clientStart'.
runClient :: ClientConfig Void -> IO WinResult
runClient conf =
  runEff
    . runErrorNoCallStackWith pure
    . runNetwork
    . runRefereeInteraction
    $ clientStart conf

-- | Entry point for a client. Called by 'runClient', it must:
--
-- 1. Establish a TCP connection with the server, backing off if the initial
-- connection failed.
--
-- 2. Transition into 'clientSetup'.
--
-- 3. Return the result of 'clientSetup', which itself is the result of the entire
-- game.
clientStart
  :: ( Network :> es
     , RefereeInteraction :> es
     , EarlyReturn WinResult :> es
     , IOE :> es -- XXX: used for runTCPClient only. Should have wrapper effect
     )
  => ClientConfig Void
  -> Eff es WinResult
clientStart conf@ClientConfig {..} =
  runTCPClient serverHostName (show serverPort) \sock -> do
    res <-
      Reader.runReader sock
        . Reader.runReader conf
        $ clientSetup
    close' sock
    pure res

-- | Setup phase of a client. Called by 'clientStart', it must:
--
-- 1. Send the name to the referee.
--
-- 2. Receive the 'EquationTable'.
--
-- 3. Transition into 'clientLoop'.
--
-- 4. Return the result of 'clientLoop', which is the end result of the entire game.
clientSetup
  :: ( RefereeInteraction :> es
     , Reader Socket :> es
     , Reader (ClientConfig Void) :> es
     , EarlyReturn WinResult :> es
     )
  => Eff es WinResult
clientSetup = do
  conf <- Reader.ask
  sock <- Reader.ask
  sendName sock (clientName conf)
  eqnT <- recvSetup
  shouldExnOnSetup conf
  clientLoop eqnT

-- | Check if this actor should throw an exception on setup. This function
-- does not itself throw an exception. However, if the actor is a looping actor,
-- it enters an infinite loop.
shouldExnOnSetup :: Applicative m => ClientConfig Void -> m ()
shouldExnOnSetup ClientConfig{actorBehavior} =
  case actorBehavior of
    WithExnActor _ _ ExnSetup -> error "Intentional exception on setup"
    WithLoopActor _ _ ExnSetup 1 -> seq bottom $ pure ()
    _ -> pure ()

-- | The main loop clients run in. Called from 'clientSetup', it must:
--
-- 1. Receive & respond to a request asking for this client to select exchanges.
--
-- 2. Receive & respond to a request asking for this client to select cards to buy.
--
-- 3. Break out of the loop if an 'RpcWin' is received.
--
-- 4. Return the result of the game.
clientLoop
  :: ( RefereeInteraction :> es
     , Reader Socket :> es
     , Reader (ClientConfig Void) :> es
     , EarlyReturn WinResult :> es
     )
  => EquationTable
  -> Eff es WinResult
clientLoop eqnT = do
  sock <- Reader.ask
  ts <- recvRequestForTrades
  (reqOrTrades, stratRes, prevHand) <- calculatePebOrTrade eqnT ts
  sendPebbleOrTrades sock reqOrTrades
  ts <- recvRequestForCardBuy
  cardBuy <- calculateCardBuy prevHand stratRes ts
  -- let newHand = hand $ playerState activePlayer
  -- sendCardBuy sock (calculateCardBuy stratRes prevHand newHand)
  sendCardBuy sock cardBuy
  updateLoopCounter (clientLoop eqnT)

calculateCardBuy
  :: ( RefereeInteraction :> es
     , Reader Socket :> es
     , Reader (ClientConfig Void) :> es
     , EarlyReturn WinResult :> es
     )
  => Hand
  -> StrategyResult
  -> TurnState Void
  -> Eff es CardBuy
calculateCardBuy prevHand stratRes ts = do
  ClientConfig {actorBehavior} <- Reader.ask
  case actorBehavior of
    WithExnActor _ _ ExnAskForCards -> error "Intentional exn in calculateCardBuy"
    WithLoopActor _ _ ExnAskForCards 1 -> bottom
    WithCheatActor _ _ cheat -> do
      maybe performStrategy pure (calculateCardBuyWithCheats cheat ts)
    _ -> performStrategy
 where
   performStrategy = do
      let newHand = hand $ playerState (activePlayer ts)
      pure
        $ evaluateCardBuyWithPreviousStrategyResult
          stratRes
          prevHand
          newHand

calculatePebOrTrade
  :: ( RefereeInteraction :> es
     , Reader Socket :> es
     , Reader (ClientConfig Void) :> es
     , EarlyReturn WinResult :> es
     )
  => EquationTable
  -> TurnState Void
  -> Eff es (PTAction, StrategyResult, Hand)
calculatePebOrTrade eqnT ts = do
  ClientConfig {actorBehavior} <- Reader.ask
  case actorBehavior of
    WithExnActor _ _ ExnAskPebbleEqn -> error "Intentional exn in calculatePebOrTrade"
    WithLoopActor _ _ ExnAskPebbleEqn 1 -> bottom
    WithCheatActor _ _ cheat -> do
      case calculatePebbleOrTradeWithCheats cheat eqnT ts of
        Nothing -> performStrategy
        Just cheatAction -> do
          (_, stratRes, prevHand) <- performStrategy
          pure (cheatAction, stratRes, prevHand)
    _ -> performStrategy
 where
   performStrategy = do
      ClientConfig {rules, strategy} <- Reader.ask
      let stratRes = runStrategy rules strategy eqnT ts
          prevHand = hand $ getPlayerState ts
      pure (stratResToPebOrTrade stratRes, stratRes, prevHand)

-- | Update the loop counter in the reader environment and evaluate the expression.
updateLoopCounter :: (Reader (ClientConfig p) :> es) => Eff es a -> Eff es a
updateLoopCounter = Reader.local decrementLoopCount
 where
  decrementLoopCount cc@ClientConfig{..} =
    let newBehavior =
          case actorBehavior of
            WithLoopActor name pp meth count -> WithLoopActor name pp meth (count - 1)
            behavior -> behavior
    in cc {actorBehavior = newBehavior}

-- | Perform the manipulations nececssary to calulate the cards this client should
-- purchase, resolving a 'NeedPebble' using the previous hand and the new hand.
evaluateCardBuyWithPreviousStrategyResult
  :: (ToPebbleSet a, ToPebbleSet b) => StrategyResult -> b -> a -> CardBuy
evaluateCardBuyWithPreviousStrategyResult stratRes prevHand newHand =
  case stratRes of
    -- PRESENTATION NOTE: k is the conventional name for "continuation" in the
    -- literature. Here, k :: Pebble -> TurnActions
    NeedPebble k ->
      -- Grab the pebble we received from the new 'TurnState'.
      -- The means for which we grab this pebble is rather suboptimal; the
      -- protocol provides what is effectively a sledgehammer of new info between
      -- our actions, rather than specifically providing the pebble we requested.
      let newPeb =
            head
              . MS.toList
              . convert @(MultiSet Pebble)
              $ newHand `difference` prevHand
       in taCardBuy $ k newPeb
    Decided actions -> taCardBuy actions

-- | Parse the 'StrategyResult' into the protocol-level EitherPebbleOrTrades.
stratResToPebOrTrade :: StrategyResult -> PTAction
stratResToPebOrTrade stratRes = case stratRes of
  NeedPebble _ -> RequestAPebble
  Decided (WithExchangeTurnActions {taExchanges}) -> Exchange taExchanges
