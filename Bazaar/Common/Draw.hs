module Bazaar.Common.Draw where

import Data.List.Split (chunksOf)
import Data.Typeable (Typeable)
import Diagrams
import Diagrams.Prelude qualified as D
import Diagrams.TwoD.Text qualified as DiText

-- | Constraint synonym for commonly used constraints in 'Draw' instances.
type DrawConstraints t b =
  ( V t ~ V2
  , N t ~ Double
  , Monoid t
  , HasOrigin t
  , Juxtaposable t
  , TrailLike t
  , Transformable t
  , HasStyle t
  , t ~ QDiagram b V2 Double D.Any
  , D.Renderable (D.Path V2 Double) b
  )

-- | Class for types that can be converted into a 2D 'Diagram'.
class (TrailLike t, V t ~ V2, N t ~ Double) => Draw a t where
  -- | Convert a value to a @t@, where @t@ is some form of diagram from "Diagrams".
  draw :: a -> t

-- | Surround the provided diagram with a rectangle @n@ units wide, @m@ units
-- tall, filling @s@ * 100 percent of the space. The diagram is centered.
enrect
  :: ( V d ~ V2
     , Semigroup d
     , Alignable d
     , HasOrigin d
     , Transformable d
     , Enveloped d
     , TrailLike d
     )
  => N d
  -> N d
  -> N d
  -> d
  -> d
enrect n m s d = d # D.centerXY # D.sized (D.dims2D (s * n) (s * m)) <> D.rect n m

-- | Draw text @t@ with fontsize @s@.
text'
  :: ( Typeable n
     , RealFloat n
     , Renderable (DiText.Text n) b
     )
  => n
  -> String
  -> QDiagram b V2 n D.Any
text' s t = DiText.text t # D.fontSize (D.local s) <> D.strutY (s * 1.3)

-- | Split up the list of @[e]@ elements into @n@ chunks, applying @f@ before
-- drawing.
splitDraw :: (Draw d t) => ([e] -> d) -> Int -> [e] -> [t]
splitDraw f n = fmap (draw . f) . chunksOf n
