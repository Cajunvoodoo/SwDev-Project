{-# LANGUAGE UndecidableInstances #-}

module Bazaar.State.TurnState where

import Bazaar.Common
import Bazaar.Player.Player
import Bazaar.Player.Reachability
import Data.Aeson (FromJSON (..), object, withObject, (.:), (.=))
import Data.List (sort)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Diagrams.Backend.Rasterific (renderRasterific)
import Diagrams.Prelude ((#), (===))
import Diagrams.Prelude qualified as D
import Diagrams.TwoD.Text qualified as DiText
import GHC.Float (int2Double)
import System.Random qualified as Random

---------------------------------------------------------------------------------

-- * TurnState

---------------------------------------------------------------------------------

-- $On the Various State Representations for Players
-- 'TurnState' contains the necessary information to execute their turn:
--
-- 1. A representation of the 'RemotePlayer', with which most actions are performed;
--
-- 2. The necessary state the player needs in order to pick their move, including
-- the current bank and the cards in play.
--
-- 'RemotePlayer' contains the information pertaining to the player itself:
--
-- 1. The means with which to /reach/ and /communicate/ with the player;
--
-- 2. The player's 'PlayerState'.
--
-- 'PlayerState' contains the game elements controlled by a player, specifically:
--
-- 1. The player's hand/wallet;
--
-- 2. The cards the player has purchased, used to calculate score.
--
-- In brief:
-- TurnState ⊃ RemotePlayer ⊃ PlayerState

-- | The TurnState includes the bank’s pebbles, the active player’s state, and
-- the current score of the remaining players. It is obtained from the
-- 'GameState', as TurnState contains strictly less information.
-- Please see @On the Various State Representations@ in "Bazaar.State.TurnState".
data TurnState p = TurnState
  { bank :: Bank
  -- ^ The pebbles in the current bank.
  , cardsInPlay :: [Card]
  -- ^ The cards currently visible to the player.
  , activePlayer :: RemotePlayer p
  -- ^ The player itself.
  , otherPlayerScores :: [Natural]
  -- ^ Scores of the other players. Order is based on the turn order; the first
  -- score in the list is the /next/ player. The last is the player who played
  -- the turn prior.
  }

deriving instance (Show (Reachability p)) => Show (TurnState p)

instance Strippable TurnState where
  strip ts = ts {activePlayer = strip (activePlayer ts)}

instance ToJSON (TurnState p) where
  toJSON TurnState {activePlayer = RemotePlayer {playerState = player}, ..} =
    object
      [ "bank" .= toPebbleSet bank
      , "cards" .= cardsInPlay
      , "active" .= player
      , "scores" .= otherPlayerScores
      ]

instance FromJSON (TurnState Void) where
  parseJSON = withObject "TurnState Pure" \v ->
    TurnState
      <$> v .: "bank"
      <*> v .: "cards"
      <*> parsePlayer v
      <*> v .: "scores"
   where
    parsePlayer v = do
      player <- v .: "active"
      let activePlayer = RemotePlayer EmptyReachability player
      pure activePlayer

instance (Eq (Reachability p)) => Eq (TurnState p) where
  (TurnState b cip ap ops) == (TurnState b' cip' ap' ops') =
    b == b' && Set.fromList cip == Set.fromList cip' && ap == ap' && ops == ops'

instance
  ( DrawConstraints t b
  , D.Renderable (DiText.Text Double) b
  , D.Renderable (D.DImage Double D.Embedded) b
  , Typeable b
  , HasName (Reachability p)
  )
  => Draw (TurnState p) t
  where
  draw TurnState {..} =
    D.hsep
      0
      [ enrect 1 2 0.9 $ D.vsep 0.25 splitBank -- bank
      , enrect 2 2 0.975 $ D.hsep 0.5 $ draw <$> sort cardsInPlay -- cards in play
      , draw activePlayer # D.translateY 0.5 # D.scale 1.33333333
      ]
      === D.hsep -- show other players scores beneath the other info
        0
        ( for otherPlayerScores \score ->
            enrect (5.0 / numOthers) 1 0.5 (text' 0.2 $ show score)
        )
      # D.alignL
      # D.translateX (-0.5)
   where
    splitBank = splitDraw toPebbleSet 8 $ convert @[Pebble] bank
    numOthers = int2Double $ length otherPlayerScores

---------------------------------------------------------------------------------

-- * TurnState Helpers and Functionality

---------------------------------------------------------------------------------

-- | Promote a 'Pure' 'TurnState' into an 'Impure' 'TurnState'.
promoteTurnState :: Reachability Impure -> TurnState Pure -> TurnState Impure
promoteTurnState conn TurnState {..} =
  TurnState
    { activePlayer = activePlayer {conn = conn}
    , ..
    }

-- | Convenience function for extracting the 'PlayerState' from the 'TurnState''s
-- 'RemotePlayer' field.
getPlayerState :: TurnState p -> PlayerState
getPlayerState TurnState {activePlayer = RemotePlayer {playerState}} = playerState

-- | Convenience function for performing an action on a 'TurnState''s
-- nested 'PlayerState'.
updatePlayerState :: (PlayerState -> PlayerState) -> TurnState p -> TurnState p
updatePlayerState f ts@TurnState {activePlayer} =
  ts {activePlayer = modifyRpState f activePlayer}

---------------------------------------------------------------------------------

-- * Examples

---------------------------------------------------------------------------------

-- TODO: remove 'turnStateDrawExample' (testing function for drawing)
turnStateDrawExample :: IO ()
turnStateDrawExample = do
  renderRasterific
    "turnstate-diagram.png"
    (D.dims2D 1600 1600)
    (draw gs)
 where
  gs =
    TurnState
      { bank = convert $ mkListOfN 50 mkRandomPebble (Random.mkStdGen 4)
      , cardsInPlay = mkListOfN 3 mkRandomCard (Random.mkStdGen 7)
      , otherPlayerScores = [10, 6, 8, 5, 7]
      , activePlayer =
          RemotePlayer
            { conn = PureComm "NAME"
            , playerState =
                PlayerState
                  { hand = convert $ mkListOfN 10 mkRandomPebble (Random.mkStdGen 2)
                  , points = 10
                  , cards = []
                  }
            }
      }
