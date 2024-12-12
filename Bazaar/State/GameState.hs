{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Bazaar.State.GameState where

import Bazaar.Common
import Bazaar.Player.Player
import Bazaar.Player.Reachability
import Bazaar.State.TurnState
import Data.Aeson
import Data.Typeable (Typeable)
import Deque.Strict qualified as Deque
import Diagrams.Backend.Rasterific (renderRasterific)
import Diagrams.Prelude (beneath, fc, lw, (#))
import Diagrams.Prelude qualified as D
import Diagrams.TwoD.Text qualified as DiText
import GHC.Exts (IsList (fromList))
import System.Random qualified as Random

---------------------------------------------------------------------------------

-- * GameState

---------------------------------------------------------------------------------

-- | Game state of /Bazaar/ used by the Referee.
data GameState p = GameState
  { bank :: Bank
  -- ^ Bag from which players draw pebbles/bank
  , cardPile :: [Card]
  -- ^ Pile from which cards are shown to players (this pile is not shown to players as-is)
  , cardsInPlay :: [Card]
  -- ^ List of cards currently in play (and shown to players)
  , players :: Deque (RemotePlayer p)
  -- ^ Deque of players, where the current head of the deque is the player whose
  -- turn it is. 'RemotePlayer' also contains the player's hand (of pebbles),
  -- their cards, and the information necessary to communicate with them
  , equations :: EquationTable
  -- ^ The Equations chosen for this game where the represenation can be filtered and
  -- equations chosen from
  }

deriving instance (Show (Reachability p)) => Show (GameState p)
deriving instance (Eq (Reachability p)) => Eq (GameState p)

instance Strippable GameState where
  strip gs = gs {players = fmap strip (players gs)}

instance FromJSON (GameState Void) where
  parseJSON = withObject "GameState" \v ->
    GameState
      <$> v .: "bank"
      <*> v .: "cards"
      <*> v .: "visibles"
      <*> parsePlayers v
      <*> pure mempty
   where
    parsePlayers v = do
      listPlayers <- v .: "players"
      let dequePlayers = fromList listPlayers
          dequeRP = fmap (RemotePlayer EmptyReachability) dequePlayers
      pure dequeRP

instance ToJSON (GameState p) where
  toJSON (GameState {..}) =
    object
      [ "bank" .= bank
      , "cards" .= cardPile
      , "visibles" .= cardsInPlay
      , "players" .= fmap playerState (toList players)
      ]

instance
  ( DrawConstraints t b
  , D.Renderable (DiText.Text Double) b
  , Typeable b
  , D.Renderable (D.DImage Double D.Embedded) b
  , HasName (Reachability p)
  )
  => Draw (GameState p) t
  where
  draw GameState {..} =
    D.vsep
      0
      [ D.hsep
          0
          [ enrect 2 3 0.9 $ D.vsep 0.25 splitBank
          , enrect 2 3 0.9 $ draw equations
          , enrect 5 3 0.975 $ D.hsep 0.5 $ fmap draw cardsInPlay
          , enrect 2 3 0.4 $ text' 0.1 remainingCards
          ]
      , enrect 11 8 0.95 playerTable # D.translateX 4.5
      ]
   where
    remainingCards = show $ length cardPile
    splitBank = splitDraw toPebbleSet 8 $ convert @[Pebble] bank
    playerTable =
      D.regPoly tableSideCnt tableSideLen
        # D.translateY (-0.75)
        # lw 0.1
        # fc D.grey
        `beneath` visPoints (D.trailVertices tableShape)
        # D.scale 1.5
    tableShape = D.regPoly tableSideCnt tableSideLen
    tableSideLen = 3
    tableSideCnt = length players
    visPoints pts = D.atPoints pts drawnPlayers
    drawnPlayers = case toList players of
      (activePlayer : otherPlayers) ->
        (draw activePlayer # D.bg D.papayawhip) : fmap draw otherPlayers
      ps -> fmap draw ps

---------------------------------------------------------------------------------

-- * GameState Helpers and Functionality

---------------------------------------------------------------------------------

-- | Obtain the 'TurnState' from a given 'GameState', thereby removing information
-- not to be revealed to 'RemotePlayer's.
-- Returns 'Nothing' when there are no players with which to generate a TurnState.
extractTurnState :: GameState p -> Maybe (TurnState p)
extractTurnState GameState {..} =
  TurnState
    <$> Just bank
    <*> Just cardsInPlay
    <*> Deque.head players
    <*> Just (toList (points . playerState <$> Deque.tail players))

-- | Modify the 'GameState' to match the fields in the provided 'TurnState'.
-- The 'players' field is modified by matching the connection of the TurnState's
-- activePlayer with the players in the GameState's 'players'.
mergeTurnState
  :: (Eq (Reachability p), Show (Reachability p))
  => TurnState p
  -> GameState p
  -> GameState p
mergeTurnState
  TurnState {bank = tsBank, cardsInPlay = tsCIP, activePlayer = tsAP}
  gs =
    gs
      { bank = tsBank
      , cardsInPlay = tsCIP
      , players = players'
      }
   where
    players' =
      dequeReplace (\RemotePlayer {conn} -> conn == tsConn) tsAP gsPlayers
    gsPlayers = players gs
    tsConn = conn tsAP
    dequeReplace p x' = fmap (\x -> if p x then x' else x)

-- | Remove the matching players from the 'GameState'.
removePlayers
  :: (Eq (Reachability p)) => [RemotePlayer p] -> GameState p -> GameState p
removePlayers (fmap conn -> removed) gs =
  gs {players = Deque.filter (\rp -> conn rp `notElem` removed) (players gs)}

-- | Like 'promoteTurnState', but for 'GameState'. If the number of 'Reachability' does
-- not match the number of players, then the remaining players are dropped.
promoteGameState :: [Reachability p'] -> GameState p -> GameState p'
promoteGameState reachabilities gs =
  gs {players = players'}
 where
  updatedPair = zip reachabilities (replicate (length reachabilities) emptyPlayerState)
  emptyPlayerState = RemotePlayer EmptyReachability (PlayerState [] 0 [])
  players' = fromList $ fmap (uncurry replaceReachability) updatedPair

---------------------------------------------------------------------------------

-- * Examples

---------------------------------------------------------------------------------

-- TODO: remove 'gameStateDrawExample' (testing function for drawing)
gameStateDrawExample :: IO ()
gameStateDrawExample =
  renderRasterific
    "gamestate-diagram.png"
    (D.dims2D 1600 1600)
    (draw $ gs 5)
 where
  gs c =
    GameState
      { bank = convert $ mkListOfN 50 mkRandomPebble (Random.mkStdGen 4)
      , cardPile = mkListOfN 10 mkRandomCard (Random.mkStdGen 3)
      , cardsInPlay = mkListOfN 3 mkRandomCard (Random.mkStdGen 7)
      , players = fmap mkRP [0 .. c - 1]
      , equations = fst $ mkRandomEquationTable (Random.mkStdGen 27)
      }
  mkRP s =
    RemotePlayer
      { conn =
          PureComm $ ["alice", "bob", "charlie", "daisy", "fuchs", "elizabeth"] !! s
      , playerState =
          PlayerState
            { hand = convert $ mkListOfN 10 mkRandomPebble (Random.mkStdGen s)
            , points = fromIntegral s * round @Double (exp (fromIntegral s + 1) / 92)
            , cards = []
            }
      }
