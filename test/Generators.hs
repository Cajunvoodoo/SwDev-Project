{-# LANGUAGE UndecidableInstances #-}

module Generators where

import Bazaar.Common.Bank
import Bazaar.Common.Cards
import Bazaar.Common.Equations
import Bazaar.Common.Equations qualified as Equation
import Bazaar.Common.Hand (Hand)
import Bazaar.Common.Internal.Prelude
import Bazaar.Common.Pebbles
import Bazaar.Common.RuleBook
import Bazaar.Player.CardMaximizer (maxCardBuyKMStrategy)
import Bazaar.Player.Mechanism
import Bazaar.Player.Player
import Bazaar.Player.PointMaximizer (maxPointBuyKMStrategy)
import Bazaar.Player.Reachability
import Bazaar.Player.Strategy
import Bazaar.State.GameState as GameState
import Bazaar.State.TurnState as TurnState
import Data.Default
import Data.Maybe qualified as Maybe
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import GHC.IsList (IsList (fromList))
import Hedgehog hiding (property)
import Hedgehog.Corpus (animals, fruits)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Random qualified as Random


-- | Generate a random 'Equation' using the provided seed, splitting it up
-- into its sides.
genEqnRandomGen'
  :: (MonadGen m, Random.RandomGen g) => g -> m (MultiSet Pebble, MultiSet Pebble)
genEqnRandomGen' g = do
  let (eqn, _g') = Equation.mkRandomEquation g
      lhsEqn = fromPebbleSet @(MultiSet Pebble) $ Equation.lhsToPebbleSet eqn
      rhsEqn = fromPebbleSet @(MultiSet Pebble) $ Equation.rhsToPebbleSet eqn
  pure (lhsEqn, rhsEqn)

-- | Generate a random 'Equation' using a random seed, splitting it up into its
-- sides.
genEqnRandomGen :: (MonadGen m) => m (MultiSet Pebble, MultiSet Pebble)
genEqnRandomGen = genStdGen >>= genEqnRandomGen'

-- | Generate a source of pseudo-randomness with a random seed.
genStdGen :: (MonadGen m) => m Random.StdGen
genStdGen = Random.mkStdGen <$> Gen.integral (Range.constant 0 maxBound)

-- | Generate a random 'Pebble'. This has better shrinking compared to
-- using 'genStdGen'.
genPebble :: (MonadGen m) => m Pebble
genPebble = Gen.element $ enumFrom (minBound @Pebble)

-- | Generate a 'PebbleSet'. This has better shrinking compared to
-- using 'genStdGen'.
genPebbleSet :: (MonadGen m) => Range Int -> m PebbleSet
genPebbleSet r = toPebbleSet <$> Gen.list r genPebble

-- | Generate a 'Hand'. This has better shrinking compared to using 'genStdGen'.
genHand :: (MonadGen m) => Range Int -> m Hand
genHand r = fromPebbleSet <$> genPebbleSet r

-- | Generate a 'Bank'. This has better shrinking compared to using 'genStdGen'.
genBank :: (MonadGen m) => Range Int -> m Bank
genBank r = fromPebbleSet <$> genPebbleSet r

-- | Generate a 'CardFace'. This has better shrinking compared to
-- using 'genStdGen'.
genCardFace :: (MonadGen m) => m CardFace
genCardFace = Gen.element $ enumFrom NoFace

-- | Generate a random 'Card'. This has better shrinking compared to
-- using 'genStdGen'.
genCard :: (MonadGen m) => m Card
genCard =
  Maybe.fromJust
    <$> (mkCard <$> genPebbleSet (Range.constant 5 5) <*> genCardFace)

-- | Generate a list of 'Card's, representing a card pile. This has better
-- shrinking compared to using 'genStdGen'.
genCardPile :: (MonadGen m) => Range Int -> m [Card]
genCardPile r = Gen.list r genCard

-- | Generate a 'RemotePlayer'. This has better shrinking compared to
-- using 'genStdGen'.
genRemotePlayer
  :: (MonadGen m) => Range Int -> Range Natural -> m (RemotePlayer Pure)
genRemotePlayer handRange scoreRange =
  RemotePlayer <$> genPureComm <*> genPlayerState handRange scoreRange

genPureComm :: (MonadGen m) => m (Reachability Pure)
genPureComm = PureComm <$> Gen.element animals

-- | Generate a 'PlayerState' with the provided range of 'Pebble's in their
-- 'Hand' and a Range for their score.
genPlayerState :: (MonadGen m) => Range Int -> Range Natural -> m PlayerState
genPlayerState handRange scoreRange =
  PlayerState
    <$> genHand handRange
    <*> Gen.integral scoreRange
    <*> pure []

-- | Generate a 'DirectionalEquation'.
genDirEquation :: (MonadGen m) => m DirectionalEquation
genDirEquation = do
  lhs <- Gen.list (Range.linear 1 4) genPebble
  rhs <-
    Gen.list
      (Range.linear 1 4)
      (Gen.filterT (not . (`elem` lhs)) genPebble)
  pure . Maybe.fromJust $ mkDirEquation lhs rhs

-- | Generate an 'Equation'.
genEquation :: (MonadGen m) => m Equation
genEquation = promoteEqn <$> genDirEquation

-- | Generate an 'Equation' and split it up into its left and right sides.
genSplitEquation :: (MonadGen m) => m (MultiSet Pebble, MultiSet Pebble)
genSplitEquation = do
  eqn <- genEquation
  pure (convert $ lhsToPebbleSet eqn, convert $ rhsToPebbleSet eqn)

-- | Generate the given range of 'Equation'. The list of equations will not
-- contain duplicate elements.
genEquations :: (MonadGen m) => Range Int -> m [Equation]
genEquations r = do
  numEqns <- Gen.integral r
  go numEqns genEquation []
 where
  go 0 _ acc = pure acc
  go n g acc = do
    eqn <- g
    let acc' = eqn : acc
    let g' = Gen.filterT (/= eqn) g
    go (n - 1) g' acc'

-- | Generate an EquationTable within the range specified. Range at size 99 must
-- result in values between 0 and 10, inclusive, otherwise an error is raised.
genEquationTable :: (MonadGen m) => Range Int -> m EquationTable
genEquationTable r =
  if Range.lowerBound 99 r >= 0 && Range.upperBound 99 r <= 10
    then Maybe.fromJust . mkEquationTable <$> genEquations r
    else error "Range to generate EquationTable must be between 0 and 10, inclusive."

-- | Options used in generation of a 'GameState'.
data GameStateOpts = GameStateOpts
  { bankRange :: Range Int
  -- ^ 0 - infinite pebbles
  , cardPileRange :: Range Int
  -- ^ 0 - infinite cards
  , cardsInPlayRange :: Range Int
  -- ^ 0 - 4 cards
  , playersRange :: Range Int
  -- ^ 2 - 6 players
  , playersHandRange :: Range Int
  -- ^ 0 - 100 pebbles in-hand
  , playersScoreRange :: Range Natural
  -- ^ 0 - 100 points
  , equationsRange :: Range Int
  -- ^ 0 - 10 equations
  }

instance Default GameStateOpts where
  def =
    GameStateOpts
      { bankRange = Range.constant 1000 1000
      , cardPileRange = Range.constant 1 10
      , cardsInPlayRange = Range.linear 0 4
      , playersRange = Range.linear 2 6
      , playersHandRange = Range.linear 0 100
      , playersScoreRange = Range.linear 0 20
      , equationsRange = Range.constant 1 10
      }

-- | Generate a pure 'GameState'. The 'equationsRange' ought not to include
-- a value larger than 10 in the range.
genGameState :: (MonadGen m) => GameStateOpts -> m (GameState Pure)
genGameState GameStateOpts {..} =
  GameState
    <$> genBank bankRange
    <*> genCardPile cardPileRange
    <*> genCardPile cardsInPlayRange
    <*> ( fromList
            <$> players (genRemotePlayer playersHandRange playersScoreRange) playersRange
        )
    <*> (Maybe.fromJust <$> (mkEquationTable <$> genEquations equationsRange))
 where
  players g r = do
    n <- Gen.integral r
    players' g n []

  players' _ 0 acc = pure acc
  players' g n acc = do
    player' <- g
    players' (Gen.filterT ((/= conn player') . conn) g) (n - 1) (player' : acc)

-- | Generate a pure 'TurnState'. Only 'bankRange', 'cardPileRange', and
-- 'playersHandRange' are considered in the generation of the @TurnState@.
-- FIXME: weird behavior when running strategies on the result of this generator
genTurnState :: (MonadGen m) => GameStateOpts -> m (TurnState Pure)
genTurnState GameStateOpts {..} =
  TurnState
    <$> genBank bankRange
    <*> genCardPile cardPileRange
    <*> genRemotePlayer playersHandRange playersScoreRange
    <*> Gen.list playersRange (Gen.integral playersScoreRange)

-- | Generate a random 'Strategy'.
genStrat :: (MonadGen m) => m (SomeStrategy, Text)
genStrat =
  Gen.choice
    [ pure (SomeStrategy maxPointBuyKMStrategy, "purchase-points")
    , pure (SomeStrategy maxCardBuyKMStrategy, "purchase-size")
    ]

-- | Generate 6 random 'Strategy's.
genStrats :: (MonadGen m) => m [(SomeStrategy, Text)]
genStrats = Gen.list (Range.singleton 6) genStrat

defWinPoints :: Natural
defWinPoints = 20

-- | Default run config for 'Rules'.
defRunRules :: Eff '[Rules Pure] a -> a
defRunRules = runPureEff . runRules 4 defWinPoints

-- | Convenient function for getting 'PureRules' from a 'GameState'.
defPureRules :: PureRules
defPureRules = defRunRules getPureRules

-- | like 'defRunRules', but uses 'genGameState' to make the 'GameState'
defRunRules' :: (MonadGen m) => Eff '[Rules Pure] a -> m a
defRunRules' eff = do
  pure $ defRunRules eff

failBoolError :: Eff '[Error e] Bool -> Bool
failBoolError eff =
  runPureEff $
    Error.runErrorNoCallStackWith (\_ -> pure False) eff

runErrorRules
  :: forall e a m
   . (MonadGen m)
  => (e -> Eff '[Rules Pure] a)
  -> Eff '[Error e, Rules Pure] a
  -> m a
runErrorRules f eff = do
  pure
    . runPureEff
    . runRules 4 defWinPoints
    $ Error.runErrorNoCallStackWith f eff

runErrorRules'
  :: (e -> Eff '[Rules Pure] c) -> Eff [Error e, Rules Pure] c -> c
runErrorRules' f eff =
  do
    runPureEff
    . runRules 4 defWinPoints
    $ Error.runErrorNoCallStackWith f eff

-- | Generate an 'ActorExn'. There is a 92% chance 'NoExn' is chosen, with every
-- other variant having a 2% chance to show up.
genActorExn :: (MonadGen m) => m ActorExn
genActorExn =
  Gen.frequency
    [ (20, pure ExnSetup)
    , (20, pure ExnAskPebbleEqn)
    , (20, pure ExnAskForCards)
    , (20, pure ExnWin)
    ]

-- | Generate a cheat used by 'Actor's.
genActorCheat :: (MonadGen m) => m ActorCheat
genActorCheat =
  Gen.frequency
    [ (20, pure CheatUseNonExistEqn)
    , (20, pure CheatBankCannotTrade)
    , (20, pure CheatWalletCannotTrade)
    , (20, pure CheatWalletCannotBuyCard)
    ]

-- | Generate an 'Actor'.
genActor :: (MonadGen m) => m Actor
genActor =
  Gen.frequency
    [ (90, genFairActor)
    , (20, genExnActor)
    , (20, genCheatActor)
    , (20, genLoopActor)
    ]

-- | Generate an 'Actor' that does not cheat/exn.
genFairActor :: (MonadGen m) => m Actor
genFairActor =
  NoExnActor
    <$> genName
    <*> genPurchasePolicy

-- | Generate an 'Actor' that throws an 'ActorExn'.
genExnActor :: (MonadGen m) => m Actor
genExnActor =
  WithExnActor
    <$> genName
    <*> genPurchasePolicy
    <*> genActorExn

-- | Generate an 'Actor' that cheats.
genCheatActor :: (MonadGen m) => m Actor
genCheatActor =
  WithCheatActor
    <$> genName
    <*> genPurchasePolicy
    <*> genActorCheat

-- | Generate an 'Actor' that loops on a specific call.
genLoopActor :: (MonadGen m) => m Actor
genLoopActor =
  WithLoopActor
    <$> genName
    <*> genPurchasePolicy
    <*> genActorExn
    <*> Gen.integral (Range.constant 1 7)

-- | Generate the name of an actor. either an animal name or a fruit.
genName :: (MonadGen m) => m Text
genName = Gen.element (animals <> fruits)

genPurchasePolicy :: (MonadGen m) => m PurchasePolicy
genPurchasePolicy =
  Gen.choice
    [ pure PurchasePoints
    , pure PurchaseSize
    ]

-- | Generate 6 'ActorExn'.
genActorExns :: (MonadGen m) => m [ActorExn]
genActorExns = Gen.list (Range.singleton 6) genActorExn

-- | Convert a list of Actors into the function used by 'addExnsPlayerInteraction'.
-- TODO: convert mapActorsToExnFunc into something more conventional e.g.,
-- cheating players
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
