{-# LANGUAGE UndecidableInstances #-}

module Bazaar.Common.Cards
  ( CardFace (..)
  , Card (face)

    -- * Construction
  , mkRandomCard
  , mkRandomCardFace
  , mkCard

    -- * Related types
  , CardBuy
  ) where

import Bazaar.Common.Draw
import Bazaar.Common.Internal.Prelude
import Bazaar.Common.Pebbles hiding (pebbles)
import Data.Aeson
import Data.FileEmbed (embedFile)
import Data.MultiSet qualified as MS
import Data.Typeable (Typeable)
import Data.Vector.Sized qualified as Sized
import Diagrams.Prelude (fc, lc, lwL, (#), (===))
import Diagrams.Prelude qualified as Diagram
import Diagrams.TwoD.Image qualified as Diagram
import System.Random qualified as Random

-- | A list of Cards to buy in order left-to-right.
type CardBuy = [Card]

-- | The face shown on a 'Card'.
data CardFace
  = NoFace
  | HappyFace
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance FromJSON CardFace where
  parseJSON = withBool "CardFace" \case
    True -> pure HappyFace
    False -> pure NoFace

instance ToJSON CardFace where
  toJSON NoFace = toJSON False
  toJSON HappyFace = toJSON True

instance
  ( DrawConstraints t b
  , Typeable t
  , Diagram.Renderable (Diagram.DImage Double Diagram.Embedded) b
  )
  => Draw CardFace t
  where
  draw = \case
    NoFace -> mempty
    HappyFace -> face
   where
    face = case Diagram.loadImageEmbBS $(embedFile "Bazaar/Other/cardFace.png") of
      Left err -> error err -- never happens; file is always read at compile time
      Right img -> Diagram.image img

-- | Given a source of randomness, generate a random 'CardFace'. A fresh generator
-- is returned.
mkRandomCardFace :: (RandomGen g) => g -> (CardFace, g)
mkRandomCardFace = first toEnum . Random.uniformR (0, fromEnum @CardFace maxBound)

-- | The Cards used in the game /Bazaar/. Always has 5 'Pebble's.
data Card = Card
  { pebbles :: Sized.Vector 5 Pebble
  , face :: CardFace
  }
  deriving stock (Show)

instance FromJSON Card where
  parseJSON = withObject "Card" \v ->
    Card
      <$> parseSizedList v
      <*> v .: "face?"
   where
    parseSizedList v = do
      list <- v .: "pebbles"
      case Sized.fromList @5 list of
        Just res -> pure res
        Nothing -> fail "Could not parse list of size 5"

instance ToJSON Card where
  toJSON Card {..} =
    object
      [ "pebbles" .= Sized.toList pebbles
      , "face?" .= face
      ]

instance Eq Card where
  (Card lPeb lFace) == (Card rPeb rFace) =
    lFace == rFace && toPebbleSet lPeb == toPebbleSet rPeb

-- | 'Card's are compared first by the presence of a 'CardFace', then by
-- their 'PebbleSet's
instance Ord Card where
  compare (Card lPeb lFace) (Card rPeb rFace) =
    case compare lFace rFace of
      EQ -> compare (toPebbleSet lPeb) (toPebbleSet rPeb)
      o -> o

instance ToPebbleSet Card where
  toPebbleSet :: Card -> PebbleSet
  toPebbleSet = toPebbleSet . MS.fromList . Sized.toList . pebbles

instance
  ( DrawConstraints t b
  , Typeable b
  , Diagram.Renderable (Diagram.DImage Double Diagram.Embedded) b
  )
  => Draw Card t
  where
  draw Card {pebbles, face} =
    rectCap
      === mconcat
        [ draw face # Diagram.scaleToX 4 # Diagram.scaleToY 4 # Diagram.translateY (-0.2)
        , visPoints $
            Diagram.trailVertices $
              Diagram.pentagon 5 # Diagram.scaleY 1.2 # Diagram.translateY (-0.2)
        , Diagram.rect bgWidth 12.7 # fc Diagram.teal # lc Diagram.black # lwL 0.1
        ]
      === rectCap
   where
    -- convert the vector to a 'PebbleSet' before extracting it to a list
    visPoints pts = Diagram.atPoints pts (fmap draw (convert @[Pebble] pebbles))
    bgWidth = 11
    rectCap = Diagram.rect bgWidth 3.2 # fc Diagram.orange # lwL 0.1

-- | Create a card given a 'PebbleSet' and the card's 'CardFace'. Fails if there
-- are not 5 elements in the PebbleSet.
mkCard :: (ToPebbleSet a) => a -> CardFace -> Maybe Card
mkCard (convert @[Pebble] -> pebs) face =
  Card <$> Sized.fromList pebs <*> Just face

-- | Given a source of randomness, generate a 'Card'. It will always have 5
-- 'Pebble's and a random 'CardFace'.
mkRandomCard :: (RandomGen g) => g -> (Card, g)
mkRandomCard g = (Card {..}, g'')
 where
  (face, g') = mkRandomCardFace g
  (pebbles, g'') = mkRandomPebbleVec @5 g'
