{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Bazaar.Common hiding (toList)
import Bazaar.Common.RuleBook
import Bazaar.Player.Mechanism
import Bazaar.Player.Player
import Bazaar.Player.Reachability
import Bazaar.Referee.Observer
import Bazaar.Referee.Referee
import Bazaar.State.GameState
import Control.Concurrent (forkIO)
import Control.Lens (at)
import Control.Lens.Operators
import Control.Lens.TH
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.FileEmbed
import Data.IORef
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Vector ((!?))
import Effectful
import GHC.IsList (IsList (..))
import Monomer
import Monomer.Lens qualified as L
import System.Directory
import System.FileLock
import System.IO
import System.IO.Silently
import XGamesCommon
import Effectful.Ki (runStructuredConcurrency)
import Effectful.Concurrent (runConcurrent)

data ObserverModel = ObserverModel
  { _obFileName :: Text
  , _obGameStateRef :: IORef (Vector (GameState Pure, Int))
  , _obGameState :: Maybe (GameState Pure)
  , _obGameStateIdx :: Int
  , _obErrorMsg :: Maybe Text
  , _obFileLock :: Maybe FileLock
  }
  deriving stock (Eq)

-- | Events used in the gui.
data ObserverEvt
  = -- | Go to the next GameState
    ObserverNext
  | -- | Go to the previous GameState
    ObserverPrev
  | -- | Save the shown GameState to the model's filePath
    ObserverSave
  | -- | Save successful
    ObserverSaveSuccess
  | -- | Save failed with <reason>
    ObserverSaveFailure Text
  | -- | Close the save error
    ObserverCloseError
  | -- | A no-op
    ObserverIgnore
  | -- | Update the model
    ObserverUpdateGs (Maybe (GameState Pure)) Int (Maybe FileLock)
  | -- | Init state, find the first image and display it
    ObserverInit
  deriving stock (Eq, Show)

instance Show FileLock where
  show _ = "FileLock"

makeLensesWith abbreviatedFields 'ObserverModel

type ObserverWenv = WidgetEnv ObserverModel ObserverEvt
type ObserverNode = WidgetNode ObserverModel ObserverEvt

main :: IO ()
main = do
  tmpExists <- doesDirectoryExist "Tmp"
  when tmpExists (removeDirectoryRecursive "Tmp")
  createDirectoryIfMissing False "Tmp"
  ioref <- newIORef mempty
  _ <- forkIO $ do
    (winnerNames, kickedNames) <- xgames ioref
    BS.putStrLn $ encode winnerNames
    BS.putStrLn $ encode kickedNames
  hSilence [stderr] $ startApp (initModel ioref) handleEvent buildUI config
 where
  initModel ioref =
    ObserverModel
      { _obFileName = ""
      , _obGameState = Nothing
      , _obErrorMsg = Nothing
      , _obGameStateIdx = -1
      , _obGameStateRef = ioref
      , _obFileLock = Nothing
      }
  config =
    [ appWindowTitle "XGames - Observer"
    , appTheme customDarkTheme
    , appFontDefMem "Regular" $(embedFile "Bazaar/Other/Roboto-Regular.ttf")
    , appInitEvent ObserverInit
    ]
  customDarkTheme =
    darkTheme
      & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"

-- | Create the actual widget tree used to display the gui.
buildUI
  :: ObserverWenv
  -> ObserverModel
  -> WidgetNode ObserverModel ObserverEvt
buildUI wenv model = widgetTree
 where
  sectionBgColor = wenv ^. L.theme . L.sectionColor

  errorOverlay = alertMsg msg ObserverCloseError
   where
    msg = Maybe.fromMaybe "" (model ^. errorMsg)

  searchForm =
    keystroke [("Enter", ObserverSave)] $
      vstack @[]
        [ hstack @[]
            [ label "Save GameState as:"
            , spacer
            , textField fileName `nodeKey` "saveas"
            , spacer
            , mainButton "Save" ObserverSave
            ]
            `styleBasic` [bgColor sectionBgColor, padding 25]
        ]

  fileNo = T.pack $ show (model ^. gameStateIdx)

  curImg = case model ^. gameState of
    Nothing ->
      zstack @[]
        [ label "Awaiting first turn (click next if it does not automatically update)"
        ]
    _ ->
      vstack @[]
        [ label $ "Turn: " <> fileNo
        , spacer
        , image ("./Tmp/" <> fileNo <> ".png")
        ]

  widgetTree =
    zstack @[]
      [ vstack @[]
          [ searchForm
          , hstack @[]
              [ curImg
              , box_ [mergeRequired (\_ _ _ -> True), alignTop, alignLeft] $
                  hstack @[]
                    [ mainButton "Next" ObserverNext
                    , spacer
                    , mainButton "Prev" ObserverPrev
                    ]
                    `styleBasic` [sizeReqH (SizeReq 30 30 0.5 0)]
              ]
          ]
      , errorOverlay `nodeVisible` Maybe.isJust (model ^. errorMsg)
      ]

-- | Handle a GUI event.
handleEvent
  :: WidgetEnv ObserverModel ObserverEvt
  -> WidgetNode ObserverModel ObserverEvt
  -> ObserverModel
  -> ObserverEvt
  -> [EventResponse ObserverModel ObserverEvt ObserverModel ObserverEvt]
handleEvent _wenv _node model evt = case evt of
  ObserverSave ->
    fmap @[]
      id
      [ Task $
          saveGameState
            (model ^. fileName)
            (model ^. gameState)
      ]
  ObserverNext ->
    [ unlockCurrentFile
    , transitionView 1
    ]
  ObserverPrev ->
    [ unlockCurrentFile
    , transitionView (-1)
    ]
  ObserverSaveFailure reason ->
    [ Model $
        model
          & errorMsg ?~ reason
    ]
  ObserverSaveSuccess ->
    [ Model $
        model
          & errorMsg ?~ ("Saved to " <> model ^. fileName <> " successfully.")
    ]
  ObserverCloseError ->
    [ Model $
        model
          & errorMsg .~ Nothing
    ]
  ObserverIgnore -> [Request RenderOnce]
  ObserverUpdateGs gs idx lock ->
    [ Model $
        model
          & gameState .~ gs
          & gameStateIdx .~ idx
          & fileLock .~ lock
    ]
  ObserverInit ->
    [ Task do
        readIORef (model ^. gameStateRef) >>= \case
          [] -> pure ObserverInit
          _ -> do
            -- don't get too eager grabbing the file. try to acquire it first.
            -- if we can acquire it, then we know it is safe to read it.
            tryLockFile "Tmp/0.png" Exclusive >>= \case
              Nothing -> pure ObserverInit
              Just lock -> unlockFile lock >> pure ObserverNext
    ]
 where
  unlockCurrentFile =
    Task do
      case model ^. fileLock of
        Nothing -> pure ObserverIgnore
        Just lock -> do
          unlockFile lock
          pure ObserverIgnore
  transitionView dIdx =
    Task $ do
      let ioref = model ^. gameStateRef
      gameStates <- readIORef ioref
      let curGs = gameStates !? (model ^. gameStateIdx + dIdx)
      case curGs of
        Nothing -> pure ObserverIgnore
        Just (gs, idx) -> do
          tryLockFile ("Tmp/" <> show idx <> ".png") Exclusive >>= \case
            Nothing -> pure ObserverIgnore
            Just lock -> do
              pure $ ObserverUpdateGs (Just gs) idx (Just lock)

saveGameState :: Text -> Maybe (GameState Pure) -> IO ObserverEvt
saveGameState "" = \_ -> pure $ ObserverSaveFailure "Filename cannot be empty."
saveGameState fileName = \case
  Nothing -> pure $ ObserverSaveFailure "Cannot save empty Game State."
  Just gs -> do
    withFile (T.unpack fileName) WriteMode $ \handle ->
      LBS.hPut handle $ encode gs
    pure ObserverSaveSuccess

-- | Run the testing task for milestone 3. Stream-parses JSON from stdin,
-- taking an EquationTable, a 'Hand', and a
xgames :: IORef (Vector (GameState Pure, Int)) -> IO ([Text], [Text])
xgames ioref = do
  streamParseJson 3 BS.getLine isEOF >>= \case
    Left fail ->
      error $ "failed to create 3-tuple of equations: " <> fail
    Right [eqnsV, exchangesV, tsV] -> do
      let actorsRes = fromJSON @[Actor] eqnsV
          eqnsRes = fromJSON @EquationTable exchangesV
          gsRes = fromJSON @(GameState Pure) tsV
      case (,,) <$> actorsRes <*> eqnsRes <*> gsRes of
        Success (actors, eqnT, gsNoEqns) -> do
          let playerNames =
                fromList $
                  setName
                    <$> zip
                      (fmap getActorName actors)
                      (toList $ players gsNoEqns)
              gs = gsNoEqns {equations = eqnT, players = playerNames}
              playersStrat = GHC.IsList.fromList (actorToRemotePlayerWithStrat <$> actors)
          (fmap getName -> winners, fmap getName -> kicked) <-
            runEff
              . runConcurrent
              . runStructuredConcurrency
              . runRules 4 20
              . runPurePlayerInteraction eqnT playersStrat
              . addExnsPlayerInteraction (mapActorsToExnFunc actors)
              . addCheatingPlayerInteraction eqnT actors
              . addLoopingPlayerInteraction actors
              . runObserverInteractionIO ioref
              . runObserverInteractionFile
              . recordGameStates
              $ refereeInit gs
          pure (List.sort winners, List.sort kicked)
        Error err -> fail err
    _ -> fail "Invalid 3-tuple"
 where
  setName (name, rp) = rp {conn = PureComm name}
