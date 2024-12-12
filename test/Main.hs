{-# HLINT ignore prop_monoidConcatenationEquationTable "Use fold" #-}
{-# HLINT ignore prop_monoidIdentityEquationTable "Monoid law, left identity"  #-}
{-# HLINT ignore prop_monoidIdentityEquationTable "Monoid law, right identity" #-}
{-# HLINT ignore prop_monoidIdentityEquationTable "Use fold" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main, recheckAt, prop_localOnlineGamesEquiv, Seed(..)) where

import Bazaar.Client.Client
import Bazaar.Client.Referee
import Bazaar.Common (Bank)
import Bazaar.Common.Cards
import Bazaar.Common.Cards qualified as Cards
import Bazaar.Common.Equations
import Bazaar.Common.Equations qualified as Equation
import Bazaar.Common.Hand (Hand)
import Bazaar.Common.Internal.Prelude hiding (toList)
import Bazaar.Common.Pebbles
import Bazaar.Common.RuleBook
import Bazaar.Common.RuleBook qualified as RuleBook
import Bazaar.Player.CardMaximizer
import Bazaar.Player.Mechanism
import Bazaar.Player.Player
import Bazaar.Player.PointMaximizer
import Bazaar.Player.Reachability
import Bazaar.Player.Strategy
import Bazaar.Referee.Referee
import Bazaar.Server.Server qualified as Server
import Bazaar.State.GameState as GameState
import Bazaar.State.TurnState as TurnState
import Control.Concurrent.STM (atomically)
import Data.Default (Default (..))
import Data.List (nub)
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.MultiSet qualified as MS
import Data.Text.Encoding qualified as T
import Data.Vector qualified as Vector
import Data.Vector.Sized qualified as Sized
import Deque.Strict qualified as Deque
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Ki (runStructuredConcurrency)
import Effectful.Writer.Static.Shared qualified as Writer
import GHC.Exts (IsList (..))
import GHC.TypeNats qualified as TypeNats
import Generators
import Hedgehog hiding (property)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ki.Unlifted qualified as Ki
import System.Exit (exitFailure)
import System.IO (hSetEncoding, utf8)
import System.Random qualified as Random
import Text.Show.Pretty (ppShow)
import qualified Control.Concurrent as IO

main :: IO ()
main = do
  hSetEncoding stdout utf8 -- the khoury servers are so jank its unbelievable.
  hSetEncoding stderr utf8
  res <- tests
  unless res exitFailure

tests :: IO Bool
tests = checkParallel $$discover

property :: PropertyT IO () -> Property
property = property' 10000

smallProp :: PropertyT IO () -> Property
smallProp = withShrinks 500 . property' 1000

unitProp :: PropertyT IO () -> Property
unitProp = property' 1

property' :: TestLimit -> PropertyT IO () -> Property
property' tl = withTests tl . Hedgehog.property

-- | Orphan instance, non-lawful, but useful to concat the Groups discovered via TH.
-- See "Hedgehog" issue #261.
instance Semigroup Group where
  (Group name lprops) <> (Group _ rprops) = Group name $ lprops <> rprops

instance Monoid Group where
  mempty = Group "" []

-- | 'PebbleSet's are alphabetically ordered by their first character when
-- rendered as a string.
prop_pebbleOrder :: Property
prop_pebbleOrder = smallProp do
  (convert @[Pebble] -> ps) <- forAll $ genPebbleSet (Range.linear 1 10)
  fmap show (List.sort ps) === List.sort (fmap show ps)

-- | 'Semigroup' instance for 'EquationTable' is lawful according to
-- associativity law.
-- @x <> (y <> z) = (x <> y) <> z@
prop_assocEquationTable :: Property
prop_assocEquationTable = smallProp do
  x <- forAll $ genEquationTable (Range.linear 0 10)
  y <- forAll $ genEquationTable (Range.linear 0 10)
  z <- forAll $ genEquationTable (Range.linear 0 10)
  x <> (y <> z) === (x <> y) <> z

-- | 'EquationTable' follows 'Monoid' laws:
-- Right identity: @x <> mempty = x@
-- Left identity: @mempty <> x = x@
prop_monoidIdentityEquationTable :: Property
prop_monoidIdentityEquationTable = smallProp do
  x <- forAll $ genEquationTable (Range.linear 0 10)
  -- right identity
  x <> mempty === x
  -- left identity
  mempty <> x === x

-- | 'EquationTable' follows 'Monoid' concatenation law:
-- @mconcat = foldr (<>) mempty@
prop_monoidConcatenationEquationTable :: Property
prop_monoidConcatenationEquationTable = smallProp do
  eqnTs <-
    forAll $ Gen.list (Range.linear 1 10) $ genEquationTable (Range.linear 0 10)
  mconcat eqnTs === foldr (<>) mempty eqnTs

-- | Make an 'Equation' property using both the Hedgehog generators and the
-- random generation supplied by Equation.
mkEquationProp
  :: (Monad m)
  => (MultiSet Pebble -> MultiSet Pebble -> PropertyT m ())
  -> PropertyT m ()
mkEquationProp prop = do
  (lhsMS, rhsMS) <- forAll genEqnRandomGen
  (lhsMS', rhsMS') <- forAll genSplitEquation
  prop lhsMS rhsMS
  prop lhsMS' rhsMS'

-- | Every generated 'Equation' ought to have /at least/ one element and
-- /at most/ 4.
prop_eqnElementCount :: Property
prop_eqnElementCount = property do
  mkEquationProp \lhsMS rhsMS -> do
    assert $ MS.distinctSize lhsMS >= 1 && MS.distinctSize lhsMS <= 4
    assert $ MS.size lhsMS >= 1 && MS.size lhsMS <= 4
    assert $ MS.distinctSize rhsMS >= 1 && MS.distinctSize rhsMS <= 4
    assert $ MS.size rhsMS >= 1 && MS.size rhsMS <= 4

-- | 'Equation's ought to share no elements in common on either side.
prop_eqnNoCommonElements :: Property
prop_eqnNoCommonElements = property do
  mkEquationProp \lhsMS rhsMS -> do
    MS.intersection lhsMS rhsMS === MS.empty

-- | 'EquationTable's ought to contain no duplicate elements. In other words,
-- for each equation in the table, there ought to be a single element with which
-- that equation is equal (itself).
prop_eqnTableNoDuplicates :: Property
prop_eqnTableNoDuplicates = property do
  g <- forAll genStdGen
  let (eqnT, _g') = Equation.mkRandomEquationTable g
      eqns = Vector.toList $ Equation.mapEquationTable id eqnT
      eqnEqualities = fmap (\eqn -> filter (== eqn) eqns) eqns
  annotateShow eqns
  assert $ all ((== 1) . length) eqnEqualities

-- | 'mkEquation' should fail if there are elements in common, and succeed
-- otherwise.
prop_mkEquationFailsOnDuplicates :: Property
prop_mkEquationFailsOnDuplicates = property do
  lhs <- forAll $ Gen.list (Range.linear 1 4) genPebble
  rhs <- forAll $ Gen.list (Range.linear 1 4) genPebble
  if hasAny lhs rhs
    then do
      label "Elements in common"
      mkDirEquation lhs rhs === Nothing
    else do
      label "No elements in common"
      let dirEqn' = mkDirEquation lhs rhs
      dirEqn <- evalMaybe dirEqn'
      toPebbleSet lhs === lhsToPebbleSet dirEqn
      toPebbleSet rhs === rhsToPebbleSet dirEqn

-- | Each side of each 'Equation' of an 'EquationSet' satisfies the 'EquationSet'.
prop_eqnSatisfiesEqnTable :: Property
prop_eqnSatisfiesEqnTable = property do
  g <- forAll genStdGen
  bank <- forAll $ genBank (Range.linear 100 1000)
  let (eqnT, _g') = Equation.mkRandomEquationTable g
      lhsEqs = Equation.mapEquationTable Equation.lhsToPebbleSet eqnT
      rhsEqs = Equation.mapEquationTable Equation.rhsToPebbleSet eqnT
  let lhsRes = fmap (Equation.anySatisfies eqnT bank) lhsEqs
  annotateShow lhsEqs
  annotateShow lhsRes
  assert $ Vector.and lhsRes
  let rhsRes = fmap (Equation.anySatisfies eqnT bank) rhsEqs
  annotateShow rhsEqs
  annotateShow rhsRes
  assert $ Vector.and rhsRes

-- | A 'Hand' with 4 of each 'Pebble' ought to be able to use any 'Equation'
-- in an 'EquationTable'.
prop_fullHandSatsifiesAllEqns :: Property
prop_fullHandSatsifiesAllEqns = property do
  g <- forAll genStdGen
  bank <- forAll $ genBank (Range.linear 100 1000)
  let (eqnT, _g') = Equation.mkRandomEquationTable g
      fullHand =
        convert @Hand
          . MS.fromOccurList
          $ fmap (,4) (enumFrom minBound :: [Pebble])
  let res = Equation.filterTable eqnT bank fullHand
      resLen = length res
  annotateShow res
  resLen === 20

-- | If one 'Card' satisfies another, they must be the same Card. Because Cards
-- are implemented with 'Sized.Vector', we ought to check the order does not
-- matter for the 'Eq' instance
prop_satisfiesCardSameCard :: Property
prop_satisfiesCardSameCard = withDiscards (toEnum maxBound) $ property do
  g <- forAll genStdGen
  let (card1, g') = Cards.mkRandomCard g
  let (card2, _g'') = Cards.mkRandomCard g'
  when (card1 /= card2) discard
  (c1SatC2, c2SatC1) <- forAll $ defRunRules' do
    pr <- getPureRules
    let c1C2 = satisfiesCard pr card1 card2
    let c2C1 = satisfiesCard pr card2 card1
    pure (c1C2, c2C1)
  assert c1SatC2
  assert c2SatC1
  card1 === card2

-- | An empty hand ought not satisfy any 'Equation's.
prop_emptyHandSatisfiesNoEqns :: Property
prop_emptyHandSatisfiesNoEqns = property do
  g <- forAll genStdGen
  bank <- forAll $ genBank (Range.linear 100 100)
  let (eqnT, _g') = Equation.mkRandomEquationTable g
      emptyHand =
        convert @Hand $
          MS.empty @Pebble
  annotateShow eqnT
  assert . null $ Equation.filterTable eqnT bank emptyHand

-- | A 'Hand' with 5 of each 'Pebble' ought to be able to buy any 'Card'.
prop_fullHandSatisfiesCard :: Property
prop_fullHandSatisfiesCard = property do
  g <- forAll genStdGen
  let (card, _g') = Cards.mkRandomCard g
      fullHand =
        fromPebbleSet @Hand
          . toPebbleSet
          . MS.fromOccurList
          $ fmap (,5) (enumFrom minBound :: [Pebble])
  annotateShow card
  annotateShow fullHand
  handSatCard <- forAll $ defRunRules' do
    pr <- getPureRules
    pure $ satisfiesCard pr fullHand card
  assert handSatCard

-- | A 'Hand' with no 'Pebble's ought not to be able to buy a 'Card'.
prop_emptyHandSatisfiesNoCard :: Property
prop_emptyHandSatisfiesNoCard = property do
  g <- forAll genStdGen
  let (card, _g') = Cards.mkRandomCard g
      emptyHand =
        fromPebbleSet @Hand
          . toPebbleSet
          $ MS.empty @Pebble
  annotateShow card
  annotateShow emptyHand
  handSatCard <- forAll $ defRunRules' do
    pr <- getPureRules
    pure $ satisfiesCard pr emptyHand card
  assert $ not handSatCard

-- | Exchange should behave as expected (@src - cost + profit@) when exchaning
-- lhs for rhs and rhs for lhs. The bank should behave in the opposite manner.
prop_exchange :: Property
prop_exchange = property do
  _bank <- forAll $ genBank (Range.linear 100 1000)
  dirEqn <- forAll genDirEquation
  let handLhs = fromPebbleSet @Hand $ Equation.lhsToPebbleSet dirEqn
      handRhs = fromPebbleSet @Hand $ Equation.rhsToPebbleSet dirEqn
      -- make sure bank has both sides in it
      bank = _bank `union` handLhs `union` handRhs
      exchangeLhsWithRhs = Equation.exchange bank handLhs dirEqn
      exchangeRhsWithLhs = Equation.exchange bank handRhs (flipDirEqn dirEqn)
  annotateShow bank
  annotateShow handLhs
  annotateShow handRhs
  annotateShow exchangeLhsWithRhs
  annotateShow exchangeRhsWithLhs
  case (exchangeLhsWithRhs, exchangeRhsWithLhs) of
    (Just (lhsExchange, lhsBank), Just (rhsExchange, rhsBank)) -> do
      let lhsPebbleSet = Equation.lhsToPebbleSet dirEqn
          rhsPebbleSet = Equation.rhsToPebbleSet dirEqn
      lhsPebbleSet === lhsBank `difference` bank
      rhsPebbleSet === rhsBank `difference` bank
      lhsPebbleSet === toPebbleSet rhsExchange
      rhsPebbleSet === toPebbleSet lhsExchange
    _ -> failure

-- | Given a 'PebbleSet', 'pickRandomPebble' should properly remove the chosen
-- element while leaving the other elements intact.
prop_pickRandomPebble :: Property
prop_pickRandomPebble = property do
  ps <- forAll $ genPebbleSet (Range.linear 0 20)
  g <- forAll genStdGen
  if isEmpty ps
    then do
      let ps' = pickRandomPebble g ps
      annotateShow ps'
      assert $ Maybe.isNothing ps'
    else do
      ((peb, ps'), _) <- evalMaybe $ pickRandomPebble g ps
      annotateShow peb
      annotateShow ps'
      assert $ peb `MS.member` convert ps
      ps' === convert (MS.delete peb (convert ps))
      assert $ ps' `isSubset` ps

-- | Buying a card should add the appropriate amount to the bank and
-- subtract the right ammount from the hand
prop_buyCard :: Property
prop_buyCard = property do
  card <- forAll genCard
  player <- forAll $ genPlayerState (Range.linear 0 10) (Range.linear 0 0)
  bank <- forAll $ genPebbleSet (Range.linear 0 100)
  let cost = toPebbleSet card
  (handSatCard, buy) <- forAll $ defRunRules' do
    pr <- getPureRules
    let handSatCard = satisfiesCard pr (hand player) card
        buy = buyCard pr bank card player
    pure (handSatCard, buy)
  if handSatCard
    then do
      case buy of
        Just (player', bank') -> do
          assert $ bank' == (bank `union` cost)
          assert $ hand player' == fromPebbleSet (hand player `difference` cost)
        _ -> assert False
    else do
      assert $ Maybe.isNothing buy

-- | 'MultiSet' ought to obey the To/FromPebbleSet laws.
prop_msRoundtrip :: Property
prop_msRoundtrip = property do
  g <- forAll genStdGen
  (len :: Natural) <- forAll $ Gen.integral (Range.linear 0 100)
  -- convert @len@ into a @KnownNat@ constraint
  TypeNats.withSomeSNat len $ \(snat :: SNat n) -> TypeNats.withKnownNat snat do
    let (pv, _) = mkRandomPebbleVec @n g
    let ms = MS.fromList $ Sized.toList pv
    fromPebbleSet @(MultiSet Pebble) (toPebbleSet pv) === ms

-- | 'Hand' ought to obey the To/FromPebbleSet laws.
prop_handRoundtrip :: Property
prop_handRoundtrip = property do
  g <- forAll genStdGen
  (len :: Natural) <- forAll $ Gen.integral (Range.linear 0 100)
  TypeNats.withSomeSNat len $ \(snat :: SNat n) -> TypeNats.withKnownNat snat do
    let (pv, _) = mkRandomPebbleVec @n g
    let ms = MS.fromList $ Sized.toList pv
    toPebbleSet (fromPebbleSet @Hand (toPebbleSet ms)) === toPebbleSet ms

-- | 'mkRandomPebbleSet' and 'mkRandomPebbleVec' ought to result in the same
-- 'PebbleVec' upon conversion.
prop_RandomPebbleSetVecEquality :: Property
prop_RandomPebbleSetVecEquality = property do
  g <- forAll genStdGen
  (len :: Natural) <- forAll $ Gen.integral (Range.linear 0 100)
  TypeNats.withSomeSNat len $ \(snat :: SNat n) -> TypeNats.withKnownNat snat do
    let (pv, _) = mkRandomPebbleVec @n g
    let (ps, _) = mkRandomPebbleSet @n g
    ps === toPebbleSet pv

-- | Equation tables ought to have length <= 10, and should fail to generate
-- if created from a list with length > 10.
prop_EquationTableLength :: Property
prop_EquationTableLength = property do
  g <- forAll genStdGen
  len <- forAll $ Gen.integral (Range.linear 0 20)
  let eqns = take len (mkListOfN 20 Equation.mkRandomEquation g)
      eqnT = Equation.mkEquationTable eqns
  annotateShow eqnT
  if len > 10 || length (nub eqns) /= len
    then assert $ Maybe.isNothing eqnT
    else do
      assert $ Maybe.isJust eqnT
      eqnT' <- evalMaybe eqnT
      let origEqns = Equation.mapEquationTable id eqnT'
      length origEqns === len

-- | 'extractTurnState' ought to fail if there are no players in the lobby and
-- ought to share the same state with the 'GameState' it originates from if there
-- /are/ players in the lobby.
prop_extractTurnState :: Property
prop_extractTurnState = property do
  gameState <-
    forAll $
      genGameState
        (def {playersRange = Range.constant 0 6, cardsInPlayRange = Range.constant 1 4})
  case players gameState of
    [] -> do
      label "Playerless GameState"
      extractTurnState gameState === Nothing
    lobby -> do
      label "Multiplayer GameState"
      lobbyActivePlayer <- evalMaybe $ Deque.head lobby
      let otherPlayers = Deque.tail lobby
          turnState' = extractTurnState gameState
      turnState <- evalMaybe turnState'
      let turnStateOtherScores = fmap (points . playerState) otherPlayers
          gameStateOtherScores =
            fmap (points . playerState) (Deque.tail $ players gameState)
      activePlayer turnState === lobbyActivePlayer
      turnStateOtherScores === gameStateOtherScores
      TurnState.bank turnState === GameState.bank gameState
      TurnState.cardsInPlay turnState === GameState.cardsInPlay gameState

----------------------------
-- GAMES THAT ARE OVER --
----------------------------

-- gamesWithoutFrontiers... wait not that one

gameOverAssert :: (MonadTest m) => GameState p -> m Bool
gameOverAssert = pure . gameIsOver defPureRules . strip

-- | Game without players should be over.
prop_gameIsOverNoPlayers :: Property
prop_gameIsOverNoPlayers = property do
  gameNoPlayers <- forAll $ genGameState (def {playersRange = Range.constant 0 0})
  assert =<< gameOverAssert gameNoPlayers

-- | Game where /a/ player has >= 20 points should be over.
prop_gameIsOverWithPoints :: Property
prop_gameIsOverWithPoints = property do
  gameWithPoints <-
    forAll $ genGameState (def {playersScoreRange = Range.constant 15 30})
  cover 90 "Player has >= 20 points" =<< gameOverAssert gameWithPoints

-- | Game without purchasable cards should be over.
prop_gameIsOverWithoutCards :: Property
prop_gameIsOverWithoutCards = property do
  gameWithoutCards <-
    forAll $
      genGameState
        ( def
            { cardsInPlayRange = Range.constant 0 0
            , cardPileRange = Range.constant 0 0
            , playersScoreRange = Range.constant 0 19
            }
        )
  assert =<< gameOverAssert gameWithoutCards

-- | Game with empty bank AND no player can buy a card should be over.
prop_gameIsOverNoBankOrBuyableCards :: Property
prop_gameIsOverNoBankOrBuyableCards = property do
  gameNoBankOrBuyableCards <-
    forAll $
      genGameState
        ( def
            { playersHandRange = Range.constant 0 0 -- can't buy, they are broke!
            , bankRange = Range.constant 0 0
            , playersScoreRange = Range.constant 0 19
            }
        )
  assert =<< gameOverAssert gameNoBankOrBuyableCards

----------------------------
-- GAMES THAT AREN'T OVER --
----------------------------

-- | Game with a bank but no buyable cards shouldn't be over.
prop_gameIsntOverWithBankNoBuyable :: Property
prop_gameIsntOverWithBankNoBuyable = property do
  gameWithBankNoBuyable <-
    forAll $
      genGameState
        ( def
            { playersHandRange = Range.constant 0 0
            , bankRange = Range.linear 10 100
            , playersScoreRange = Range.constant 0 19
            }
        )
  let pr = defPureRules
  assert . not $ gameIsOver pr $ strip gameWithBankNoBuyable

-- | Game with empty bank but buyable cards should be over.
prop_gameIsntOverEmptyBankAndBuyable :: Property
prop_gameIsntOverEmptyBankAndBuyable = property do
  gameWithEmptyBankAndBuyable <-
    forAll $
      genGameState
        ( def
            { playersHandRange = Range.linear 10 30
            , bankRange = Range.constant 0 0
            , playersScoreRange = Range.constant 0 19
            , cardPileRange = Range.linear 0 10
            , cardsInPlayRange = Range.constant 1 4
            , playersRange = Range.constant 2 6
            }
        )
  cover 90 "Player can buy a card" . not
    =<< gameOverAssert gameWithEmptyBankAndBuyable

-- | Simple example with specific test case.
prop_pointMaximizeSmall :: Property
prop_pointMaximizeSmall = unitProp do
  let
    examplePoints :: (Rules Pure :> es) => Eff es StrategyResult
    examplePoints = do
      pr <- getPureRules
      pure $ runStrategy pr maxPointBuyKMStrategy eqnT turnState
    -- let examplePoints = runStrategy maxPointBuyKMStrategy eqnT turnState
    expected =
      Decided $
        WithExchangeTurnActions
          { taExchanges = [eqn1]
          , taCardBuy = [noFaceCard, faceCard]
          }
  defRunRules examplePoints === expected
 where
  faceCard =
    Maybe.fromJust $
      mkCard ([RedPbl, BluePbl, BluePbl, BluePbl, BluePbl] :: [Pebble]) HappyFace
  noFaceCard =
    Maybe.fromJust $
      mkCard ([YellowPbl, BluePbl, BluePbl, BluePbl, BluePbl] :: [Pebble]) NoFace
  turnState =
    TurnState
      { bank = convert $ mkListOfN 10000 mkRandomPebble (Random.mkStdGen 4)
      , cardsInPlay =
          [ faceCard
          , noFaceCard
          ]
      , otherPlayerScores = [10, 6, 8, 5, 7]
      , activePlayer =
          RemotePlayer
            { conn = PureComm "alice"
            , playerState =
                PlayerState
                  { hand =
                      convert
                        ([YellowPbl, RedPbl, RedPbl, BluePbl, BluePbl, BluePbl, BluePbl] :: [Pebble])
                  , points = 10
                  , cards = []
                  }
            }
      }
  eqnT = Maybe.fromJust $ mkEquationTable [promoteEqn eqn1]
  eqn1 = Maybe.fromJust $ mkDirEquation [RedPbl] (replicate 4 BluePbl)

mkMaximizerComparison
  :: (Strategy a, Strategy b)
  => a
  -> b
  -> ( EquationTable
       -> GameState Pure
       -> TurnState Pure
       -> PlayerState
       -> Bank
       -> StrategyResult
       -> StrategyResult
       -> PropertyT IO ()
     )
  -> Property
mkMaximizerComparison lStrat rStrat prop = withDiscards 2000 . withShrinks 700 $ smallProp do
  gameState <-
    forAll $
      genGameState
        ( def
            { bankRange = Range.constant 1000 1000
            , cardPileRange = Range.constant 10 10
            , cardsInPlayRange = Range.linear 1 4
            , playersRange = Range.constant 1 1
            , playersHandRange = Range.linear 0 20
            , playersScoreRange = Range.constant 0 0
            , equationsRange = Range.linear 1 10
            }
        )
  turnState <- evalMaybe $ extractTurnState gameState
  let bank = TurnState.bank turnState
  let run = do
        pr <- getPureRules
        let RemotePlayer {playerState = player} = activePlayer turnState
            lRes = runStrategy pr lStrat (equations gameState) turnState
            rRes = runStrategy pr rStrat (equations gameState) turnState
        pure (lRes, rRes, player)
  let (lRes, rRes, player) = defRunRules run
  prop (equations gameState) gameState turnState player bank lRes rRes

-- | Maximizing points should always yield points equal to or greater than a
-- card-maximizing strategy.
prop_pointMaximizeGreaterThanCardMaximize :: Property
prop_pointMaximizeGreaterThanCardMaximize =
  mkMaximizerComparison
    maxPointBuyKMStrategy
    maxCardBuyKMStrategy
    \_eqnT _gs _ts player@PlayerState {hand} bank pointRes cardRes ->
      case (pointRes, cardRes) of
        (Decided pointActions, Decided cardActions) -> do
          let pointExchanges = getExchanges pointActions
              cardExchanges = getExchanges cardActions
              pointBuys = taCardBuy pointActions
              cardBuys = taCardBuy cardActions
          annotate $ ppShow pointRes
          annotate $ ppShow cardRes

          let pr = defPureRules
              evalPEqns = rer $ evalEqns (hand, bank) pointExchanges
              evalCEqns = rer $ evalEqns (hand, bank) cardExchanges
          (pH', pB') <- evalMaybe evalPEqns
          (cH', cB') <- evalMaybe evalCEqns
          let pPlayer' = player {hand = pH'}
              cPlayer' = player {hand = cH'}
              evalPFinal = evalBuys pr (pPlayer', pB') pointBuys
              evalCFinal = evalBuys pr (cPlayer', cB') cardBuys
          (pPlayerFinal, _pBFinal) <- evalMaybe evalPFinal
          (cPlayerFinal, _cBFinal) <- evalMaybe evalCFinal
          annotateShow pPlayerFinal
          annotateShow cPlayerFinal
          diff (points pPlayerFinal) (>=) (points cPlayerFinal)
        _ -> discard
 where
  rer = runErrorRules' id
  getExchanges (WithExchangeTurnActions ts _) = ts
  getExchanges _ = []

-- | Maximizing points should always yield points equal to or greater than a
-- card-maximizing strategy.
prop_cardMaximizeAlwaysMoreCards :: Property
prop_cardMaximizeAlwaysMoreCards =
  mkMaximizerComparison
    maxPointBuyKMStrategy
    maxCardBuyKMStrategy
    \_ _ _ _ _ pointRes cardRes ->
      case (pointRes, cardRes) of
        (Decided pointActions, Decided cardActions) -> do
          let pointBuys = taCardBuy pointActions
              cardBuys = taCardBuy cardActions
          annotateShow pointRes
          annotateShow cardRes
          diff (length cardBuys) (>=) (length pointBuys)
        _ -> discard

-- | 'performPTAction' ought to properly reject invalid exchanges.
prop_verifyPTAction :: Property
prop_verifyPTAction = withShrinks 200 $ smallProp do
  eqnT <- forAll $ genEquationTable (Range.linear 1 10)
  gameState <- forAll $ genGameState def
  turnState <- evalMaybe $ extractTurnState gameState
  dirEqns <- forAll $ Gen.list (Range.linear 1 3) genDirEquation
  let exchanges = Exchange dirEqns
      eqnTDirEqns = unwrapEquationTable eqnT
      expected = all ((`elem` eqnTDirEqns) . promoteEqn) dirEqns
      PlayerState {hand} = getPlayerState turnState
      TurnState {bank} = turnState
      Exchange trades = exchanges
  -- eqnsRes = evalEqns (hand, bank) trades
  (res, fmap snd -> eqnsRes) <- forAll $ runErrorRules (\_ -> pure (False, Nothing)) do
    (,)
      <$> (RuleBook.performPTAction eqnT turnState exchanges >> pure True)
      <*> evalEqns (hand, bank) trades
  if res
    then do
      assert expected
      void $ evalMaybe eqnsRes
    else do
      assert $
        not expected
          || Maybe.isNothing eqnsRes

-- | 'performCardBuy' ought to reject invalid card purchases.
prop_verifyCardAction :: Property
prop_verifyCardAction = withShrinks 200 $ property do
  gameState <- forAll $ genGameState (def {cardsInPlayRange = Range.linear 1 4})
  turnState <- evalMaybe $ extractTurnState gameState
  cardBuy <-
    forAll $
      Gen.list
        (Range.linear 1 4)
        (Gen.choice (pure <$> TurnState.cardsInPlay turnState))
  let (res, eqnsRes) =
        runErrorRules' (\_ -> pure (False, Nothing)) do
          pr <- getPureRules
          void $ performCardBuy pr turnState cardBuy
          let eqnsRes = evalBuys pr (ps, bank) cardBuy
          pure (True, eqnsRes)
      ps = getPlayerState turnState
      TurnState {bank, cardsInPlay} = turnState
      expected = all (`elem` cardsInPlay) cardBuy
  annotateShow cardsInPlay
  annotateShow cardBuy
  if res
    then do
      assert expected
      void $ evalMaybe eqnsRes
    else do
      assert $
        not expected
          || Maybe.isNothing eqnsRes
  cover 40 "Valid CardBuy" res
  cover 40 "Invalid CardBuy" (not res)

-- | Unit test asserting the correct order of drawn pebbles.
prop_pureGameRulePebbleOrder :: Property
prop_pureGameRulePebbleOrder = unitProp do
  gs <- forAll $ genGameState def
  let GameState {bank} =
        gs
          { bank =
              convert @Bank @[Pebble]
                [ RedPbl
                , BluePbl
                , BluePbl
                , RedPbl
                , GreenPbl
                , YellowPbl
                , BluePbl
                , WhitePbl
                , BluePbl
                , WhitePbl
                ]
          }
          :: GameState Pure
      expectedOrder :: [Pebble]
      expectedOrder =
        [ RedPbl
        , RedPbl
        , WhitePbl
        , WhitePbl
        , BluePbl
        , BluePbl
        , BluePbl
        , BluePbl
        , GreenPbl
        , YellowPbl
        ]
      foldFunc (Just (_, bank')) _ = drawPebble bank'
      foldFunc _ _ = pure Nothing
  let (_, pebblesDrawn) =
        defRunRules
          . ( impose (Writer.runWriter @[Pebble]) \_ -> \case
                GetPureRules -> undefined
                RemoveCardFromPile _ -> undefined
                DrawPebble bank -> do
                  pick <- send $ DrawPebble bank
                  case pick of
                    Nothing -> pure Nothing
                    Just (peb, bank') -> do
                      Writer.tell @[Pebble] [peb]
                      pure $ Just (peb, bank')
            )
          $ foldlM
            (foldFunc)
            (Just (RedPbl, bank))
            expectedOrder
  pebblesDrawn === expectedOrder

-- | Unit test for running a full game with a kicked player, a loser, and a winner.
-- This is a golden test, meaning the example itself is unimportant. This test
-- is intended to be a canary in a coal mine; when the results don't match the
-- example, something is buggy.
prop_regressionGameStateGoldenTest :: Property
prop_regressionGameStateGoldenTest = withShrinks 50 $ unitProp do
  let gs = mkGs 6
  let pointStrat = SomeStrategy maxPointBuyKMStrategy
      cardStrat = SomeStrategy maxCardBuyKMStrategy
      strats = [pointStrat, cardStrat, cardStrat, cardStrat, cardStrat, cardStrat]
  let ((winners, kicked), losers) =
        runPureEff
          . runRules 20 4
          . runPlayerInteraction strats gs
          . ( impose (Writer.runWriter @[RemotePlayer Pure]) \_ -> \case
                MarkTurn gs -> send (MarkTurn @Pure gs)
                KickPlayer v p -> send (KickPlayer @Pure v p)
                SetupPlayer e p -> send (SetupPlayer @Pure e p)
                AskPebbleOrTrades ts@TurnState {activePlayer} ->
                  case conn activePlayer of
                    PureComm "alice" ->
                      pure . Just $
                        RuleBook.Exchange
                          [fst . splitEquation . fst . mkRandomEquation $ Random.mkStdGen 6]
                    _ -> send (AskPebbleOrTrades @Pure ts)
                AskForCardBuy ts -> send (AskForCardBuy @Pure ts)
                Win p b -> do
                  unless b do
                    Writer.tell @[RemotePlayer Pure] [p]
                  send (Win @Pure p b)
                GetWinnersAndKicked gs -> send (GetWinnersAndKicked @Pure gs)
            )
          $ refereeInit gs
      expectedWinner = [PureComm "bob"]
      expectedLosers =
        List.sort
          [ PureComm "charlie"
          , PureComm "daisy"
          , PureComm "elizabeth"
          , PureComm "felix"
          ]
      expectedKicked = [PureComm "alice"]
  annotateShow winners
  annotateShow losers
  annotateShow kicked
  let winnerNames = List.sort (conn <$> winners)
      loserNames = List.sort (conn <$> losers)
      kickedNames = List.sort (conn <$> kicked)
  winnerNames === expectedWinner
  loserNames === expectedLosers
  kickedNames === expectedKicked
 where
  mkGs c =
    GameState
      { bank = convert $ mkListOfN 100 mkRandomPebble (Random.mkStdGen 4)
      , cardPile = mkListOfN 16 mkRandomCard (Random.mkStdGen 3)
      , cardsInPlay = mkListOfN 4 mkRandomCard (Random.mkStdGen 7)
      , players = fmap mkRp [0 .. c - 1]
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
            { hand = convert $ mkListOfN 0 mkRandomPebble (Random.mkStdGen s)
            , points = 0
            , cards = []
            }
      }

-- | An overly lenient 'GameStateOpts'.
--
-- XXX: The game states this generator configuration creates may violate some
-- invariants. This is intended: rather than strictly test invariant-conforming
-- scenarios, we opt to violate them to explore more code. It is a tradeoff we
-- believe is better suited for property testing, as the quality of our generated
-- examples is paramount to achieving good coverage.
lenientGsConf :: GameStateOpts
lenientGsConf =
  def
    { bankRange = Range.linear 0 100
    , cardPileRange = Range.linear 0 16
    , cardsInPlayRange = Range.linear 1 4
    , playersRange = Range.linear 2 6
    , playersHandRange = Range.linear 0 10
    , playersScoreRange = Range.linear 0 20
    , equationsRange = Range.linear 0 10
    }

-- | A strict 'GameStateOpts'. Used when outputing tests to a file. Unlike
-- 'lenientGsConf', this generator configuration only generates invariant-conforming
-- GameStates.
strictGsConf :: GameStateOpts
strictGsConf =
  def
    { bankRange = Range.linear 0 70
    , cardPileRange = Range.linear 0 16
    , cardsInPlayRange = Range.singleton 4
    , playersRange = Range.linear 3 6
    , playersHandRange = Range.linear 0 5
    , playersScoreRange = Range.linear 0 20
    , equationsRange = Range.linear 0 10
    }

-- | All games, no matter what they contain, should terminate with our strategies.
-- "Logical Results" means:
--
-- * No winners are also losers.
--
-- * No winners are duplicated in the winning player list.
--
-- * No kicked players are duplicated in the kicked player list.
--
-- * When there are no winners (and there were players when the game started),
-- then there must be at least kicked player.
prop_gamesTerminateWithLogicalResults :: Property
prop_gamesTerminateWithLogicalResults = withShrinks 250 $ smallProp do
  gs <- forAll $ genGameState lenientGsConf
  let numActors = length (players gs)
  actorsUnnamed <- forAll $ Gen.list (Range.singleton numActors) genActor
  let actors =
        fmap
          (\(actor, rp) -> setName actor (getName rp))
          (zip actorsUnnamed (toList $ players gs))
  let strats = fmap (SomeStrategy . policyToStrategy . getActorPurchasePolicy) actors

  (winners, kicked) <-
    liftIO
      . runEff
      . runConcurrent
      . runStructuredConcurrency
      . runRules 20 4
      . runPlayerInteraction strats gs
      . addExnsPlayerInteraction (mapActorsToExnFunc actors)
      . addCheatingPlayerInteraction (equations gs) actors
      . addLoopingPlayerInteraction def actors
      $ refereeInit gs

  annotateShow winners
  annotateShow kicked
  let winnerNames = fmap getName winners
  let kickedNames = fmap getName kicked

  assert . not $ hasAny winnerNames kickedNames
  nubOrd winnerNames === winnerNames
  nubOrd kickedNames === kickedNames
  when (null winners) do
    label "Winners Kicked"
    assert . not $ null kicked

-- | Running a local game without cheaters and running a remote game without
-- cheaters should have the same results (winners and losers). This property
-- does not check individual calls, but it would not be hard to add this capability.
prop_localOnlineGamesEquiv :: Property
prop_localOnlineGamesEquiv = withShrinks 50 $ property' 250 do
  (gs, actors, strats) <- forAll $ genActorGs lenientGsConf
  let names = List.sort . fmap getName
  (names -> localWinners, names -> localKicked) <- runGameWithActors gs actors strats
  let actorsWithStrats = zip actors strats
  ((List.sort -> remoteWinners, List.sort -> remoteKicked), clientResults) <-
    liftIO $ runRemoteGameWithActors gs actorsWithStrats
  annotateShow localWinners
  annotateShow localKicked
  annotateShow remoteWinners
  annotateShow remoteKicked
  annotateShow clientResults

  remoteWinners === localWinners
  remoteKicked === localKicked

  -- All self-proclaimed winners ought to be in the localWinners and remoteWinners
  -- lists, and no self-proclaimed losers should be in these lists.
  forM_ clientResults \(actor, winRes) -> do
    case winRes of
      Winner -> do
        label "Contains Winning Clients"
        assert $ getName actor `elem` localWinners
        assert $ getName actor `elem` remoteWinners
        assert $ getName actor `notElem` localKicked
        assert $ getName actor `notElem` remoteKicked
      Loser -> do
        label "Contains Losing Clients"
        assert $ getName actor `notElem` localWinners
        assert $ getName actor `notElem` remoteWinners

-- TODO: tripping property for Rpc decoding and encoding

-- | Delay between spawning the server and the clients and between spawning
-- each individual client. Prevents race conditions on who connects first.
delayBetweenForks :: Int
delayBetweenForks = 10000

-- TODO: move runRemoteGameWithActors to Generators (or some other module).

-- | Given a 'GameState' and actors, set up a remote game and return the results.
runRemoteGameWithActors
  :: GameState Pure
  -> [(Actor, SomeStrategy)]
  -> IO (([Text], [Text]), [(Actor, WinResult)])
runRemoteGameWithActors gs actorsWithStrats = do
  Ki.scoped \scope -> do
    serverResultT <- Ki.fork scope (Server.runBazaarSeeded def 42069 gs)
    IO.threadDelay delayBetweenForks
    clientResultsT <- spawnClients scope actorsWithStrats
    (serverResult, clientResults) <-
      atomically
        ((,) <$> Ki.await serverResultT <*> mapM Ki.await clientResultsT)
    pure (serverResult, clientResults)

-- | Spawn each remote client. Used by 'runRemoteGameWithActors'.
spawnClients
  :: Ki.Scope -> [(Actor, SomeStrategy)] -> IO [Ki.Thread (Actor, WinResult)]
spawnClients scope actors = do
  forM actors \(actor, strat) -> do
    let name = getName actor
    let client = mkLocalClient (T.encodeUtf8 name) strat 42069 actor
    -- BUG: clients are not cheating because we do not handle special Actors
    -- there.
    IO.threadDelay delayBetweenForks
    Ki.fork scope ((,) actor <$> runClient client)

-- | Handle the necessary effects to run a local game with cheating/looping/exn
-- 'Actor's.
runGameWithActors
  :: (MonadIO m)
  => GameState Pure
  -> [Actor]
  -> [SomeStrategy]
  -> m ([RemotePlayer Pure], [RemotePlayer Pure])
runGameWithActors gs actors strats =
  liftIO
    . runEff
    . runConcurrent
    . runStructuredConcurrency
    . runRules 20 4
    . runPlayerInteraction strats gs
    . addExnsPlayerInteraction (mapActorsToExnFunc actors)
    . addCheatingPlayerInteraction (equations gs) actors
    . addLoopingPlayerInteraction def actors
    $ refereeInit gs

genActorGs :: MonadGen m => GameStateOpts -> m (GameState Pure, [Actor], [SomeStrategy])
genActorGs gsConf = do
  gs <- genGameState gsConf
  let gsPlayers = toList $ players gs
  let numActors = length gsPlayers
  actorsUnnamed <- Gen.list (Range.singleton numActors) genFairActor -- genActor
  let actors =
        fmap (\(actor, rp) -> setName actor (getName rp)) (zip actorsUnnamed gsPlayers)
  let strats = fmap getStrat actors
  pure (gs, actors, strats)

getStrat :: Actor -> SomeStrategy
getStrat = SomeStrategy . policyToStrategy . getActorPurchasePolicy

setName :: Actor -> Text -> Actor
setName actor name = case actor of
  NoExnActor _ pp -> NoExnActor name pp
  WithExnActor _ pp exn -> WithExnActor name pp exn
  WithCheatActor _ pp cheat -> WithCheatActor name pp cheat
  WithLoopActor _ pp exn count -> WithLoopActor name pp exn count

runPlayerInteraction
  :: (Rules Pure :> es)
  => [SomeStrategy]
  -> GameState Pure
  -> Eff (PlayerInteraction Pure : es) a
  -> Eff es a
runPlayerInteraction strats gs =
  runPurePlayerInteraction (equations gs) playersWithStrat
 where
  players' = players gs
  playersWithStrat =
    fromList $ zip (toList players') (cycle strats)
