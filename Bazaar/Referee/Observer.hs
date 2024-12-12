module Bazaar.Referee.Observer where

import Bazaar.Common
import Bazaar.Player.Mechanism
import Bazaar.State.GameState
import Data.IORef
import Data.Vector qualified as Vector
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared qualified as State
import Effectful.TH
import Data.ByteString.Lazy qualified as LBS
import Diagrams.Backend.Rasterific (rasterRgb8)
import Diagrams.Prelude qualified as D
import Bazaar.Player.Reachability
import System.FileLock
import Codec.Picture

-- | Effect to allow interaction with observers.
data ObserverInteraction p :: Effect where
  -- | Notify observers of a specific 'GameState'.
  NotifyObservers :: GameState p -> ObserverInteraction p m ()

makeEffect ''ObserverInteraction

-- | Write the 'GameState's (and the turn number) into the provided 'IORef'
-- whenever 'NotifyObservers' is called.
runObserverInteractionIO
  :: (IOE :> es)
  => IORef (Vector (GameState p, Int))
  -> Eff (ObserverInteraction p : es) a
  -> Eff es a
runObserverInteractionIO ioref = interpret_ \case
  NotifyObservers gs -> do
    curStates <- liftIO $ readIORef ioref
    let lastIdx = case Vector.unsnoc curStates of
          Nothing -> 0
          Just (_, (_, lastIdx)) -> lastIdx + 1
    liftIO $ modifyIORef ioref (`Vector.snoc` (gs, lastIdx))

-- | Records the turns taken via the 'ObserverInteraction' effect.
recordGameStates
  :: forall p es a
   . (PlayerInteraction p :> es, ObserverInteraction p :> es)
  => Eff es a
  -> Eff es a
recordGameStates = interpose_ \case
  MarkTurn gs -> do
    notifyObservers gs
    send (MarkTurn @p gs)
  GetWinnersAndKicked gs -> do
    send (GetWinnersAndKicked @p gs)
  KickPlayer violation player -> do
    send (KickPlayer @p violation player)
  SetupPlayer eqnT player -> do
    send (SetupPlayer @p eqnT player)
  AskPebbleOrTrades ts -> do
    send (AskPebbleOrTrades @p ts)
  AskForCardBuy ts -> do
    send (AskForCardBuy @p ts)
  Win player isWinner -> do
    send (Win @p player isWinner)

-- | Write the 'GameState's to a file whenever they are received.
runObserverInteractionFile
  :: forall p es a
   . (IOE :> es, ObserverInteraction p :> es, HasName (Reachability p))
  => Eff es a
  -> Eff es a
runObserverInteractionFile = impose_ (State.evalState (0 :: Int)) \case
  NotifyObservers gs -> do
    send (NotifyObservers @p gs)
    idx <- State.get
    State.modify (+ 1)
    let fileName = "Tmp/" <> show idx <> ".png"
        encodedImg = encodePng $ rasterRgb8 (D.dims2D imageX imageY) (draw gs D.# D.bg D.white)
    liftIO $ withFileLock fileName Exclusive \lock -> do
      LBS.writeFile fileName encodedImg
      unlockFile lock

imageX, imageY :: Double
imageX = 400
imageY = 400
