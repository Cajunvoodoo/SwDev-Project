{-# LANGUAGE UndecidableInstances #-}

module Bazaar.Player.Player where

import Bazaar.Common
import Bazaar.Player.Reachability
import Data.Aeson
import Data.Text qualified as T
import Diagrams.Backend.Rasterific (renderRasterific)
import Diagrams.Prelude qualified as D
import Diagrams.TwoD.Text qualified as DiText
import System.Random qualified as Random
import Control.Monad.Extra (fromMaybeM)

-- | 'PlayerState' contains the game elements controlled by a player, specifically:
-- * The player's hand/wallet;
-- * The cards the player has purchased, used to calculate score.
data PlayerState = PlayerState
  { hand :: !Hand
  -- ^ The pebbles this player owns.
  , points :: !Natural
  -- ^ The points this player has obtained through buying cards.
  , cards :: ![Card]
  -- ^ The cards the player has purchased.
  }
  deriving stock (Show, Eq)

instance FromJSON PlayerState where
  parseJSON = withObject "PlayerState" \v ->
    PlayerState
      <$> v .: "wallet"
      <*> v .: "score"
      <*> cards v
   where
    cards v = fromMaybeM (pure mempty) (v .:? "cards")

instance ToJSON PlayerState where
  toJSON :: PlayerState -> Value
  toJSON PlayerState {..} =
    object
      [ "wallet" .= toPebbleSet hand
      , "score" .= points
      ]

-- | 'RemotePlayer' contains the information pertaining to the player itself:
-- * The means with which to /reach/ and /communicate/ with the player;
-- * The player's 'PlayerState'.
data RemotePlayer p = RemotePlayer
  { conn :: Reachability p
  , playerState :: PlayerState
  }

deriving instance (Eq (Reachability p)) => Eq (RemotePlayer p)
deriving instance (Show (Reachability p)) => Show (RemotePlayer p)

instance (HasName (Reachability p)) => HasName (RemotePlayer p) where
  getName :: (HasName (Reachability p)) => RemotePlayer p -> Text
  getName RemotePlayer {conn} = getName conn

instance Strippable RemotePlayer where
  strip :: RemotePlayer a -> RemotePlayer Void
  strip = replaceReachability EmptyReachability

instance
  ( DrawConstraints t b
  , D.Renderable (DiText.Text Double) b
  , D.Renderable (D.Path V2 Double) b
  , D.Renderable (D.DImage Double D.Embedded) b
  , HasName (Reachability p)
  )
  => Draw (RemotePlayer p) t
  where
  draw rp@RemotePlayer {playerState = PlayerState {..}} =
    D.vsep
      0
      [ enrect 1.5 0.5 0.6 $ text' 0.1 (T.unpack (getName rp) <> ": " <> show points)
      , enrect 1.5 1 0.95 $ D.vsep 0.25 splitWallet
      ]
   where
    splitWallet = splitDraw toPebbleSet 5 (convert @[Pebble] hand)

-- | Obtain the points for the provided 'RemotePlayer'.
calcPointsForPlayer :: RemotePlayer p -> Natural
calcPointsForPlayer RemotePlayer {playerState = PlayerState {points}} = points

-- | Replace the 'Reachability' of the 'RemotePlayer'.
replaceReachability :: Reachability p' -> RemotePlayer p -> RemotePlayer p'
replaceReachability conn' rp = rp {conn = conn'}

-- | Apply a function to a 'RemotePlayer''s inner 'PlayerState'.
modifyRpState
  :: (PlayerState -> PlayerState) -> RemotePlayer p -> RemotePlayer p
modifyRpState f rp = rp {playerState = f (playerState rp)}

-- TODO: remove 'playerStateDrawExample' (testing function for drawing)
playerStateDrawExample :: IO ()
playerStateDrawExample =
  renderRasterific
    "test-diagram.png"
    (D.dims2D 1600 1600)
    (draw gs)
 where
  gs =
    RemotePlayer
      { conn = PureComm "NAME"
      , playerState =
          PlayerState
            { hand = convert $ mkListOfN 10 mkRandomPebble (Random.mkStdGen 1)
            , points = 10
            , cards = []
            }
      }
