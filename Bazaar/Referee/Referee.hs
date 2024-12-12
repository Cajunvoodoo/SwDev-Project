{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Bazaar.Referee.Referee where

import Bazaar.Common hiding (toList)
import Bazaar.Common.RuleBook
import Bazaar.Player.Mechanism
import Bazaar.Player.Player
import Bazaar.Player.PointMaximizer (maxPointBuyKMStrategy)
import Bazaar.Player.Reachability
import Bazaar.Player.Strategy
import Bazaar.State.GameState
import Bazaar.State.TurnState
import Data.Maybe qualified as Maybe
import Data.Vector qualified as Vector
import Effectful
import Effectful.Exception qualified as Exception
import GHC.IsList (IsList (..))
import System.Random qualified as Random
import qualified Data.Text as T

-- | Used as the result of 'EarlyReturn' to indicate a Player's actions ended the
-- game during the middle of the turn.
newtype EndGameMidTurn p = EndGameMidTurn {finalGameState :: GameState p}

deriving instance (Show (Reachability p)) => Show (EndGameMidTurn p)

-- | Type synonym to describe broken rules and the 'GameState' they were broken in.
type RuleBreak p = (PlayerViolation, GameState p)

---------------------------------------------------------------------------------

-- * Game Setup Logistics

---------------------------------------------------------------------------------

-- | Entry point for the referee. Performs the setup actions, then calls 'referee'.
-- The returned players are in no particular order.
refereeInit
  :: ( Eq (Reachability p)
     , Show (Reachability p)
     , Rules p :> es
     , PlayerInteraction p :> es
     , HasName (Reachability p)
     )
  => GameState p
  -> Eff es ([RemotePlayer p], [RemotePlayer p])
refereeInit gs = do
  gs' <- setupPlayers gs
  referee gs'

-- | Send the 'EquationTable' to each player with 'setup', kicking them if they
-- throw an exception. Returns the 'GameState' without these players.
setupPlayers
  :: ( PlayerInteraction p :> es
     , Show (Reachability p)
     , Eq (Reachability p)
     , HasName (Reachability p)
     )
  => GameState p
  -> Eff es (GameState p)
setupPlayers gs@GameState {equations, players} = do
  -- if we cannot set them up, kick them and remove them from the game
  let setup' eqnT rp = Exception.handleSync (\e -> pure $ Just (ExceptionThrown e, rp)) do
        setupPlayer eqnT rp
        pure Nothing
  kicked <- Maybe.catMaybes <$> mapM (setup' equations) (toList players)
  kickPlayers gs kicked

---------------------------------------------------------------------------------

-- * Game Loop

---------------------------------------------------------------------------------

-- | The outcomes of a single turn.
data TurnResult p
  = -- | Kick the current player and replay the turn from the specified 'GameState'.
    Rerun PlayerViolation (RemotePlayer p) (GameState p)
  | -- | Continue as normal and play the next turn.
    Continue (GameState p)

-- | Run the referee with the given 'GameState'. Terminates so long as no turn
-- limit is present and players do not collude to extend the game.
referee
  :: ( Eq (Reachability p)
     , Rules p :> es
     , PlayerInteraction p :> es
     , Show (Reachability p)
     , HasName (Reachability p)
     )
  => GameState p
  -- ^ initial 'GameState'
  -> Eff es ([RemotePlayer p], [RemotePlayer p])
  -- ^ (winners, kicked players)
referee gs = do
  pr <- getPureRules
  case gameIsOver' pr gs of
    Nothing -> endGameSendWin gs
    Just ts -> do
      playTurn pr ts gs >>= \case
        Rerun violation activePlayer gs' -> do
          gs'' <- kickPlayers gs' [(violation, activePlayer)]
          referee gs''
        Continue gs' -> referee gs'

-- | End the game by running 'win' on each 'RemotePlayer', kicking them if they
-- throw an exception.
endGameSendWin
  :: (PlayerInteraction p :> es, Eq (Reachability p), HasName (Reachability p))
  => GameState p
  -> Eff es ([RemotePlayer p], [RemotePlayer p])
endGameSendWin gs = do
  markTurn gs
  (winners, kickedPlayers) <- getWinnersAndKicked gs
  let losers = filter (not . (`elem` winners)) (toList $ players gs)
  -- if telling them they've won fails, mark them as kicked and remove them
  -- from the list of winners
  let win' rp w = Exception.handleSync (const . pure $ Just rp) do
        win rp w
        pure Nothing
  kickedWinners <- Maybe.catMaybes <$> mapM (`win'` True) winners
  kickedLosers <- Maybe.catMaybes <$> mapM (`win'` False) losers
  let winners' = winners `remove` kickedWinners
  pure (winners', kickedPlayers <> kickedLosers <> kickedWinners)

---------------------------------------------------------------------------------

-- ** Playing a Turn

---------------------------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | Play a single turn with the provided 'PureRules', the 'TurnState' of this
-- turn, and the 'GameState' the 'TurnState' was extracted from.
-- The 'GameState' is used to merge the actions of Players into a new 'GameState'
-- returned as a part of the 'TurnResult'.
playTurn
  :: forall p es. ( Eq (Reachability p)
     , PlayerInteraction p :> es
     , Rules p :> es
     , Show (Reachability p)
     )
  => PureRules
  -- ^ 'PureRules' of the game
  -> TurnState p
  -- ^ 'TurnState' to play
  -> GameState p
  -- ^ 'GameState' to merge with
  -> Eff es (TurnResult p)
playTurn pr@PureRules {..} ts gs =
  -- See :DeepDarkErrorImplementationNote
  -- We make these two distinctions because they serve distinct purposes
  -- and they catch two different types.
  -- They do not impose meaningful performance penalties.
  onEarlyReturn (uncurry (kickOnViolation ts))
  . onEarlyReturn endGamePrematurely
  $ do
    -- Note: all of this exception stuff is unnecessary. Our effects do
    -- not throw exceptions. They are *artificially* introduced in the effect
    -- handler purely to check the box for the milestones. If we could remove it,
    -- we would!
    markTurn gs
    ptAction <- exnAt gs (unwrapOrKick gs =<< askPebbleOrTrades ts)
    gs       <- handlePTAction ptAction ts gs
    ts       <- extractTsOrEndGame gs
    cardBuy  <- exnAt gs (unwrapOrKick gs =<< askForCardBuy ts)
    gs <- handleCardBuy pr cardBuy ts gs
    let nextGs = pickNextPlayer $ refreshCardsInPlay gs
    pure $ Continue nextGs
 where
  endGamePrematurely = pure . Continue . finalGameState
  -- | Specialization of 'returnMaybe'. Throw a 'TimedOut' when the received value
  -- is 'Nothing'.
  unwrapOrKick gs = returnMaybe (TimedOut, gs)
  -- | Catch exceptions, contextualize them with 'ExceptionThrown' and the 'GameState'.
  exnAt gs = contextualizeExn (\e -> (ExceptionThrown e, gs))

  -- | Function to 'Rerun' a turn, blaming the active player with the provided
  -- violation.
  kickOnViolation
    :: (Applicative f)
    => TurnState p
    -> PlayerViolation
    -> GameState p
    -> f (TurnResult p)
  kickOnViolation ts v gs = pure $ Rerun v (activePlayer ts) gs
{- FOURMOLU_ENABLE -}

---------------------------------------------------------------------------------

-- *** 'playTurn' Helper Functions

---------------------------------------------------------------------------------

-- | Extract the 'TurnState', ending the game if that fails.
extractTsOrEndGame
  :: (EarlyReturn (EndGameMidTurn p) :> es)
  => GameState p
  -- ^ 'GameState' to extract from
  -> Eff es (TurnState p)
extractTsOrEndGame gs = do
  case extractTurnState gs of
    Nothing -> returnEarly_ (EndGameMidTurn $ gs {players = mempty})
    Just ts -> pure ts

-- | Perform the 'CardBuy' and merge the result into a new 'GameState'.
-- A 'PlayerViolation' is raised when the 'CardBuy' cannot be performed.
handleCardBuy
  :: ( EarlyReturn (RuleBreak p) :> es
     , Rules p :> es
     , Eq (Reachability p)
     , Show (Reachability p)
     )
  => PureRules
  -> CardBuy
  -- ^ The 'Card's to buy
  -> TurnState p
  -- ^ 'TurnState' the current turn belongs to
  -> GameState p
  -- ^ 'GameState' to merge
  -> Eff es (GameState p)
  -- ^ Merged 'GameState'
handleCardBuy pr cb ts gs = do
  ts <- contextualizeWith gs $ performCardBuy pr ts cb
  pure $ mergeTurnState ts gs

-- | Perform a 'PTAction' and merge the result into a new 'GameState'.
-- A 'PlayerViolation' is raised when the 'PTAction' cannot be performed.
-- A 'EndGameMidTurn' is raised if an exchange is made and there are no cards
-- in the 'GameState' cardPile.
handlePTAction
  :: ( Eq (Reachability p)
     , Rules p :> es
     , EarlyReturn (EndGameMidTurn p) :> es
     , EarlyReturn (RuleBreak p) :> es
     , Show (Reachability p)
     )
  => PTAction
  -- ^ The 'PTAction' performed
  -> TurnState p
  -- ^ The 'TurnState' the 'PTAction' was performed in
  -> GameState p
  -- ^ The 'GameState' to merge
  -> Eff es (GameState p)
handlePTAction ptAction ts gs@GameState {equations} = do
  pr <- getPureRules
  ts <- contextualizeWith gs $ performPTAction equations ts ptAction
  mergePTActionResult pr gs ts ptAction

-- | Perform the logistics of the 'PTAction' (e.g., removing a card from the
-- cardPile), then merge the result into a new 'GameState'.
mergePTActionResult
  :: ( Eq (Reachability p)
     , Rules p :> es
     , EarlyReturn (EndGameMidTurn p) :> es
     , Show (Reachability p)
     )
  => PureRules
  -> GameState p
  -> TurnState p
  -> PTAction
  -> Eff es (GameState p)
mergePTActionResult PureRules {..} gs ts = \case
  Exchange _ -> do
    gs' <- removeCardFromPile gs
    returnWhen (EndGameMidTurn gs') (gameIsOver $ strip gs')
    pure $ mergeTurnState ts gs'
  _ -> pure $ mergeTurnState ts gs

---------------------------------------------------------------------------------

-- * Examples

---------------------------------------------------------------------------------

exampleFullGame :: IO ([RemotePlayer Pure], [RemotePlayer Pure])
exampleFullGame = do
  let gs = mkGs 6
  let res =
        runPureEff $
          runRefereePure 200 (replicate 6 $ SomeStrategy maxPointBuyKMStrategy) gs
  pure res
 where
  mkGs c =
    GameState
      { bank = convert $ mkListOfN 100 mkRandomPebble (Random.mkStdGen 4)
      , cardPile = mkListOfN 150 mkRandomCard (Random.mkStdGen 3)
      , cardsInPlay = mkListOfN 4 mkRandomCard (Random.mkStdGen 7)
      , players = fmap mkRp (fromList [0 .. c - 1])
      , equations =
          Maybe.fromJust
            . mkEquationTable
            . Vector.toList
            . Vector.take 10
            . unwrapEquationTable
            . fst
            $ mkRandomEquationTable (Random.mkStdGen 27)
      }
  mkRp s =
    RemotePlayer
      { conn =
          PureComm $ ["alice", "bob", "charlie", "daisy", "elizabeth", "felix"] !! s
      , playerState =
          PlayerState
            { hand = convert $ mkListOfN 20 mkRandomPebble (Random.mkStdGen s)
            , points = 0
            , cards = []
            }
      }

runRefereePure
  :: forall es
   . Natural
  -> [SomeStrategy]
  -> GameState Pure
  -> Eff es ([RemotePlayer Pure], [RemotePlayer Pure])
runRefereePure targetScore strats gs =
  runRules 4 targetScore
    . runPlayerInteraction
    $ refereeInit gs
 where
  -- runPlayerInteraction :: Eff (PlayerInteraction Pure : es) a -> Eff es a
  runPlayerInteraction =
    runPurePlayerInteraction (equations gs) playersWithStrat
   where
    players' = players gs
    playersWithStrat =
      fromList $ zip (toList players') (cycle strats)
