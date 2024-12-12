{-# LANGUAGE NoOverloadedLists #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Bazaar.Common.RuleBook where

import Bazaar.Common.Bank
import Bazaar.Common.Cards
import Bazaar.Common.Equations
import Bazaar.Common.Hand
import Bazaar.Common.Internal.Prelude hiding (readFile, writeFile)
import Bazaar.Common.Pebbles
import Bazaar.Player.Player
import Bazaar.Player.Reachability (Strippable (..))
import Bazaar.State.GameState
import Bazaar.State.GameState qualified as GameState
import Bazaar.State.TurnState
import Data.List qualified as List
import Deque.Strict qualified as Deque
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import Effectful.TH
import Prelude hiding (readFile, writeFile)

--------------------------------------------------------------------------------

-- * PlayerViolation

--------------------------------------------------------------------------------

-- | Illegal moves made by a player, parameterized by the context in which the
-- rule was broken in.
data PlayerViolation
  = IllegalDraw Bank
  | IllegalEquations Hand Trades
  | IllegalCardBuy Hand CardBuy
  | ExceptionThrown SomeException
  | TimedOut
  deriving stock (Show)

instance Exception PlayerViolation

-- | Helper for 'maybeToError' regarding 'PlayerViolation's.
mkPTActionViolation :: Bank -> Hand -> PTAction -> PlayerViolation
mkPTActionViolation bank hand = \case
  RequestAPebble -> IllegalDraw bank
  Exchange trades -> IllegalEquations hand trades

--------------------------------------------------------------------------------

-- * PTAction

--------------------------------------------------------------------------------

-- | Play Turn Action. Players either request a pebble or make exchanges.
data PTAction
  = RequestAPebble
  | Exchange Trades
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- * Rules Effect

--------------------------------------------------------------------------------

-- | Rules of the game, accompanying the 'Rules' effect. Holds rules that do not
-- rely on effectful actions.
data PureRules = PureRules
  { gameIsOver :: GameState Void -> Bool
  -- ^ Is the game over?
  , calculatePoints' :: forall a. (ToPebbleSet a) => a -> Card -> Natural
  -- ^ Calculate the points gained from buying the provided Card
  -- with the hand @a@ /after/ the purchase. That is, the pebbles used to
  -- purchase the Card have already been deducted from @a@.
  , satisfiesCard :: forall a. (ToPebbleSet a) => a -> Card -> Bool
  -- ^ Given some value and a 'Card', determine whether the card can be purchased
  -- with the provided value's 'PebbleSet'.
  , refreshCardsInPlay :: forall p. GameState p -> GameState p
  -- ^ Refresh the cards visible to players.
  , pickNextPlayer :: forall p. GameState p -> GameState p
  -- ^ Pick the next player to take a turn.
  , pointGoal :: Natural
  -- ^ The number of points a player must have to immediately win the game.
  }

-- | Default implementation of 'PureRules'.
defaultPureRules :: Int -> Natural -> PureRules
defaultPureRules maxInPlay winPts =
  PureRules
    { gameIsOver = \gs ->
        or
          [ allPlayersKicked gs
          , anyPlayerReachedPointGoal (defaultPureRules maxInPlay winPts) gs
          , noCardsAvailableForPurchase gs
          , bankIsEmptyAndCantBuyCards (defaultPureRules maxInPlay winPts) gs
          ]
    , calculatePoints' = \(sizePS -> n) card ->
        let pointsFromRem n min low med hi
              | n == 0 = hi
              | n == 1 = med
              | n == 2 = low
              | otherwise = min
         in case face card of
              NoFace -> pointsFromRem n 1 2 3 5
              HappyFace -> pointsFromRem n 2 3 5 8
    , satisfiesCard = \a (toPebbleSet -> pebbles) ->
        pebbles `isSubset` a
    , refreshCardsInPlay = \gs ->
        case cardPile gs of
          [] -> gs
          cip -> do
            let countToReplace = maxInPlay - length (GameState.cardsInPlay gs)
                replacements = take countToReplace cip
                cardPile' = drop countToReplace cip
                cardsInPlay' = GameState.cardsInPlay gs <> replacements
             in gs {GameState.cardsInPlay = cardsInPlay', cardPile = cardPile'}
    , pickNextPlayer = \gs ->
        let players' = Deque.shiftLeft $ players gs
         in gs {players = players'}
    , pointGoal = winPts
    }
 where
  allPlayersKicked GameState {..} = Deque.null players

  anyPlayerReachedPointGoal PureRules {pointGoal} GameState {..} =
    any ((>= pointGoal) <$> calcPointsForPlayer) players

  noCardsAvailableForPurchase GameState {..} = null cardPile && null cardsInPlay

  bankIsEmptyAndCantBuyCards pr GameState {..} =
    null (convert @[Pebble] bank)
      && not (any (playerCanBuyCard pr cardsInPlay) players)

  -- \| Determine if the given player can buy any of the cards in the collection.
  playerCanBuyCard PureRules {..} cards RemotePlayer {playerState = PlayerState {hand}} =
    any (satisfiesCard hand) cards

data Rules p :: Effect where
  -- | Draw a 'Pebble' from the 'Bank', which may fail.
  DrawPebble :: Bank -> Rules p m (Maybe (Pebble, Bank))
  -- | Remove a card from the pile of invisible cards.
  RemoveCardFromPile :: GameState p -> Rules p m (GameState p)
  GetPureRules :: Rules p m PureRules

makeEffect ''Rules

--------------------------------------------------------------------------------

-- * Rules Helpers

--------------------------------------------------------------------------------

-- | Is the game over? If not, extract the 'TurnState'.
gameIsOver' :: PureRules -> GameState p -> Maybe (TurnState p)
gameIsOver' PureRules {..} gs = do
  case (gameIsOver (strip gs), extractTurnState gs) of
    (False, Just ts) -> Just ts
    _ -> Nothing

-- | Calculate the points of the given purchase, assuming the provided PebbleSet
-- has enough pebbles to make the purchase.
calculatePoints
  :: (ToPebbleSet a, FromPebbleSet a)
  => PureRules
  -> a
  -> CardBuy
  -> (a, Natural)
calculatePoints PureRules {..} ps cb =
  first fromPebbleSet $
    foldl'
      ( \(ps', pts) c -> do
          let ps'' = ps' `difference` c
              calcPoints = calculatePoints'
              ptsGained = calcPoints ps'' c
          (ps'', pts + ptsGained)
      )
      (toPebbleSet ps, 0)
      cb

-- | Buy the given card and adds the cost to the bank while subtracting from
-- the player's hand.
buyCard
  :: (ToPebbleSet bank, FromPebbleSet bank)
  => PureRules
  -> bank
  -> Card
  -> PlayerState
  -> Maybe (PlayerState, bank)
buyCard PureRules {..} bank card player
  | h `satisfiesCard` card = Just (player', bank')
  | otherwise = Nothing
 where
  h = hand player
  cost = toPebbleSet card
  player' =
    player
      { hand = fromPebbleSet h'
      , points = points player + calculatePoints' h' card
      , cards = cards player <> [card]
      }
  bank' = fromPebbleSet $ bank `union` cost
  h' = h `difference` cost

-- :DeepDarkErrorImplementationNote - Effectful uses /precise exceptions/ in its
-- interpretation of the 'Error' effect. Precise exceptions are /cheaper/ than
-- the Either/Maybe monad in terms of short-circuiting. GHC implements precise
-- exceptions /as if/ it were implemented with the Either monad. Thus, for
-- short-circuiting behavior such as ending a game early or booting a
-- misbehaving player, 'Error' is appropriate. N.B. the delimited continuations
-- primitives in GHC utilize very similar behavior; its not slow or expensive!

-- | Like 'drawPebble', but throws an 'Error' when a pebble cannot be drawn.
drawPebble'
  :: (Rules p :> es, EarlyReturn PlayerViolation :> es)
  => Bank
  -> Eff es (Pebble, Bank)
drawPebble' bank = drawPebble bank >>= returnMaybe (IllegalDraw bank)

-- | Using the provided initial 'Hand' and 'Bank', apply the 'DirectionalEquation's
-- in order left-to-right, returning the resulting 'Hand' and 'Bank' if successful.
evalEqns
  :: (Foldable f, Rules p :> es)
  => (Hand, Bank)
  -> f DirectionalEquation
  -> Eff es (Maybe (Hand, Bank))
evalEqns (hand, bank) = foldlM evalEqn (Just (hand, bank))
 where
  evalEqn Nothing _ = pure Nothing
  evalEqn (Just (hand, bank')) eqn = pure $ exchange bank' hand eqn

-- | Using the provided initial 'PlayerState' and 'Bank', buy the 'Card's in order
-- left-to-right, returning the resulting 'PlayerState' and 'Bank' if successful.
evalBuys
  :: (Foldable f)
  => PureRules
  -> (PlayerState, Bank)
  -> f Card
  -> Maybe (PlayerState, Bank)
evalBuys pr = foldlM (\(player', bank') card -> buyCard pr bank' card player')

-- | Like 'evalEqns', but raises a 'PlayerViolation' when the equations are
-- not in the table and cannot be afforded.
evalEqns'
  :: (EarlyReturn PlayerViolation :> es, Rules p :> es)
  => EquationTable
  -> (Hand, Bank)
  -> Trades
  -> Eff es (Hand, Bank)
evalEqns' eqnT (hand, bank) dirEqns =
  returnMaybeM
    (mkPTActionViolation bank hand $ Exchange dirEqns)
    ( if allInTable
        then evalEqns (hand, bank) dirEqns
        else pure Nothing
    )
 where
  allInTable = all (inTable eqnT) dirEqns

-- | Like 'evalBuys', but raises a 'PlayerViolation' when the cards cannot be
-- bought.
evalBuys'
  :: (EarlyReturn PlayerViolation :> es, Rules p :> es)
  => PureRules
  -> TurnState p
  -> (PlayerState, Bank)
  -> CardBuy
  -> Eff es (PlayerState, Bank)
evalBuys' pr TurnState {cardsInPlay} (ps@PlayerState {hand}, bank) cb = do
  returnMaybeM
    (IllegalCardBuy hand cb)
    ( if allInPlay
        then pure $ evalBuys pr (ps, bank) cb
        else pure Nothing
    )
 where
  allInPlay = all (`elem` cardsInPlay) cb

-- | Perform a PT Action, potentially returning a new 'TurnState'.
performPTAction
  :: forall p es
   . (Rules p :> es, EarlyReturn PlayerViolation :> es)
  => EquationTable
  -> TurnState p
  -> PTAction
  -> Eff es (TurnState p)
performPTAction eqnT ts@TurnState {..} action = do
  let PlayerState {hand} = getPlayerState ts
  case action of
    RequestAPebble -> do
      (convert -> peb, bank') <- drawPebble' bank
      let ts' = updatePlayerState (\ps -> ps {hand = hand <> peb}) ts
      pure (ts' {bank = bank'} :: TurnState p)
    Exchange trades -> do
      (hand', bank') <- evalEqns' eqnT (hand, bank) trades
      let ts' = updatePlayerState (\ps -> ps {hand = hand'}) ts
      pure $ ts' {bank = bank'}

-- | Perfrom a 'CardBuy', raising a 'PlayerViolation' if the action is illegal.
-- Returns a 'TurnState' with the appropriate fields updated.
performCardBuy
  :: forall p es
   . (EarlyReturn PlayerViolation :> es, Rules p :> es)
  => PureRules
  -> TurnState p
  -> CardBuy
  -> Eff es (TurnState p)
performCardBuy pr ts@TurnState {..} cb = do
  let ps = getPlayerState ts
  (ps', bank') <- evalBuys' pr ts (ps, bank) cb
  let ts' =
        ts
          { bank = bank'
          , cardsInPlay = cardsInPlay `remove` cb
          }
          :: TurnState p
  pure $ updatePlayerState (const ps') ts'

--------------------------------------------------------------------------------

-- * EFFECT HANDLER --

--------------------------------------------------------------------------------

-- | Run the 'Rules' effect purely. Pebbles are drawn in order Red, White, Blue,
-- Green, Yellow. The drawing is /not/ stateful.
runRules
  :: forall p es a
   . Natural
  -- ^ Max number of cards in play at any given time
  -> Natural
  -- ^ Points needed to win
  -> Eff (Rules p : es) a
  -> Eff es a
runRules (toIntegral @Int -> maxInPlay) winPts = interpret_ \case
  GetPureRules ->
    pure (defaultPureRules maxInPlay winPts)
  RemoveCardFromPile gs -> do
    case cardPile gs of
      [] -> pure (gs {cardsInPlay = mempty} :: GameState p)
      cs -> pure $ gs {cardPile = init cs}
  DrawPebble (convert @[Pebble] -> bank) -> do
    case bank of
      [] -> pure Nothing
      pebs -> do
        peb <- calcCurrentPbl bank
        pure $ Just (peb, convert $ List.delete peb pebs)
 where
  findValid :: [Pebble] -> Pebble -> Pebble
  findValid bank desired =
    if desired `elem` bank
      then desired
      else findValid bank $ nextPbl desired
  calcCurrentPbl bank = pure $ findValid bank RedPbl
  nextPbl RedPbl = WhitePbl
  nextPbl WhitePbl = BluePbl
  nextPbl BluePbl = GreenPbl
  nextPbl GreenPbl = YellowPbl
  nextPbl YellowPbl = RedPbl
