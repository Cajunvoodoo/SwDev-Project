{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
module Bazaar.Server.Server where

import Bazaar.Common hiding (toList)
import Bazaar.Common.RuleBook
import Bazaar.Player.Mechanism
import Bazaar.Player.Player
import Bazaar.Player.Reachability
import Bazaar.Referee.Referee
import Bazaar.State.GameState
import Control.Monad.Extra
import Data.Bifunctor (bimap)
import Data.List qualified as List
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import Effectful.Ki (StructuredConcurrency, runStructuredConcurrency)
import Effectful.Network as Network
import Effectful.TH
import GHC.IsList (IsList (..))
import Network.Socket (defaultHints)
import System.Random
import Text.RE.TDFA
import Data.Default (def)

-- | Effect for generating a 'GameState'.
data GameStateGeneration :: Effect where
  GenGameState :: [Reachability p] -> GameStateGeneration m (GameState p)

makeEffect ''GameStateGeneration

-- | Handle the 'GameStateGeneration' effect using a randomly generated 'GameState'.
runDefaultGameStateGenerationImpure
  :: (IOE :> es) => Eff (GameStateGeneration : es) a -> Eff es a
runDefaultGameStateGenerationImpure = interpret_ \case
  GenGameState reachabilities -> do
    let numPlayers = length reachabilities
    let pebblesPerPlayer = 5
    let mkRandPebs =
          convert . take pebblesPerPlayer . List.unfoldr (Just . mkRandomCard)
            <$> getStdGen
    let mkPlayer = do
          hand <- mkRandPebs
          pure $ PlayerState hand 0 []
    playerStates <- replicateM numPlayers mkPlayer
    let bank = convert . concat $ replicate ((100 - (pebblesPerPlayer * numPlayers)) `div` length pebbles) pebbles
        players =
          fromList
            [ RemotePlayer conn ps
            | (ps, conn) <- zip playerStates reachabilities
            ]
    cardPile <- mkListOfN 16 mkRandomCard <$> getStdGen
    cardsInPlay <- mkListOfN 4 mkRandomCard <$> getStdGen
    equations <- fst . mkRandomEquationTable <$> getStdGen
    pure $ GameState {..}

-- | Handle the 'GameStateGeneration' effect using a static 'GameState'.
-- Note the 'genGameState' function will apply its received 'Reachability's to the
-- provided 'GameState'. If the number of Reachabilities
runPresetGameStateGeneration
  :: GameState p -> Eff (GameStateGeneration : es) a -> Eff es a
runPresetGameStateGeneration gs = interpret_ \case
  GenGameState reachabilities -> pure $ promoteGameState reachabilities gs

-- | Run a game using 'startLobby'. If the first lobby created has too few players,
-- then the players are kept and another lobby is formed. If this lobby similarly
-- times out, then the game is never ran and 'mempty' is returned. Otherwise,
-- the game is run using 'refereeInit'.
runLobbiedGame
  :: ( Rules Impure :> es
     , PlayerInteraction Impure :> es
     , Network :> es
     , NetworkRun :> es
     , StructuredConcurrency :> es
     , Concurrent :> es
     , GameStateGeneration :> es
     )
  => TimeoutConf
  -> Int
  -- ^ Port to run the game on.
  -> Int
  -- ^ Minimum number of players, inclusive.
  -> Int
  -- ^ Maximum number of players, inclusive.
  -> Eff es ([Text], [Text])
runLobbiedGame timeoutConf port minPlayers maxPlayers = do
  reachabilities <-
    startLobby timeoutConf port maxPlayers >>= \case
      rps | True <- minPlayers < length rps -> pure rps
      rps -> startLobby' timeoutConf rps port maxPlayers
  gs <- genGameState reachabilities
  res <- if length reachabilities < minPlayers
    then pure mempty
    else bimap (fmap getName) (fmap getName) <$> refereeInit gs
  forM_ reachabilities \(ImpureComm sock _) -> do
    close sock
  pure res

-- | Run the Bazaar Server with randomly generated games. The output names are
-- in no particular order; the user of this function may wish to sort the names
-- to comply with other parties' interests.
runBazaar :: Int -> IO ([Text], [Text])
runBazaar port =
  runEff
    . runNetworkRun
    . runNetwork
    . runConnectionImpure
    . runConcurrent
    . runStructuredConcurrency
    . runRules 4 20
    . runImpurePlayerInteraction timeoutConf
    . runDefaultGameStateGenerationImpure
    $ runLobbiedGame timeoutConf port 1 6
 where
  timeoutConf = def

-- | Run the Bazaar Server with a preset game and a bonus function. Returned
-- names are *not* sorted. The caller ought to sort the names. The output names
-- are in no particular order; the user of this function may wish to sort the
-- names to comply with other parties' interests.
runBazaarSeededWithBonus
  :: (MonadIO m)
  => TimeoutConf
  -> Int -- ^ Port to host the server on.
  -> (forall es p a. PlayerInteraction p :> es => Eff es a -> Eff es a)
     -- ^ Bonus function, universally quanitified over the effects, purity, and
     -- return type. Usually this interposes on 'GetWinnersAndKicked'.
  -> GameState p
  -> m ([Text], [Text])
runBazaarSeededWithBonus timeoutConf port bonusFunc gs =
  liftIO
    . runEff
    . runNetworkRun
    . runNetwork
    . runConnectionImpure
    . runConcurrent
    . runStructuredConcurrency
    . runRules 4 20
    . runImpurePlayerInteraction timeoutConf
    . runPresetGameStateGeneration gs
    . bonusFunc
    -- Maximum number of players is relative to GameState because this function
    -- is used for testing, and we would have to wait the lobby timeout otherwise.
    $ runLobbiedGame timeoutConf port 1 6 -- (length $ players gs)

-- | Run the Bazaar Server with a preset game. Returned names are *not* sorted.
-- The caller ought to sort the names. Takes the port to host the server on.
runBazaarSeeded :: (MonadIO m) => TimeoutConf -> Int -> GameState p -> m ([Text], [Text])
runBazaarSeeded timeoutConf port = runBazaarSeededWithBonus timeoutConf port id

-- | Start a lobby, waiting at most 20 seconds for players to join. A player has
-- 2 seconds to respond with their name. If their name is invalid (according to the
-- regex @^[a-zA-Z0-9]{1,20}$@) or if they fail to respond in time, the connection
-- will be closed. If the player sends a valid name but later disconnects, they
-- will be kicked once the game is set up. Returns the players in the lobby.
-- This function will error if the port is invalid (taken, illegal, etc.).
startLobby'
  :: (Network :> es, NetworkRun :> es, StructuredConcurrency :> es, Concurrent :> es)
  => TimeoutConf
  -> [Reachability Impure]
  -> Int
  -- ^ Port to listen for players on.
  -> Int
  -- ^ Maximum number of players in the game.
  -> Eff es [Reachability Impure]
startLobby' TimeoutConf {..} rps port maxPlayers = do
  let hints = defaultHints {addrFamily = AF_INET, addrSocketType = Stream}
  addrInfo <-
    head <$> getAddrInfo (Just hints) (Just "0.0.0.0") (Just $ show port)
  lobbySock <- openServerSocket addrInfo
  listen lobbySock 6 -- 6 maximum in queue waiting to be accepted
  receivedPlayers <- newTVarIO rps
  let canAcceptMorePlayers count = count < maxPlayers
  timeout (millisToMicros lobbyFillTimeout) do
    -- while we can accept more players...
    whileM do
      (canAcceptMorePlayers . length <$> readTVarIO receivedPlayers) >>= \case
        False -> pure False -- if we can't accept any more, break from the loop
        True -> do -- otherwise, accept connection, then wait for their name
          bracketOnError (Network.accept lobbySock) (Network.close . fst) \(conn, _peer) -> void do
            race nameRecvDelay do
              setSocketOption conn ReuseAddr 1 -- resource consolidation during tests
              maybeName <- recvLatinText conn 1024 -- 1kb of data max
              case maybeName of
                Nothing -> Network.close conn
                Just name ->
                  if matched (name ?=~ [re|^[a-zA-Z0-9]{1,20}$|])
                    then do
                      let comm = ImpureComm conn name
                      atomically $ modifyTVar receivedPlayers (comm :)
                    else Network.close conn
          pure True
  Network.close lobbySock
  reverse <$> readTVarIO receivedPlayers
 where
  nameRecvDelay = threadDelay (millisToMicros nameRecvTimeout)

-- | Like 'startLobby', but with an empty list of starting 'Reachability'.
startLobby
  :: ( Network :> es
     , NetworkRun :> es
     , StructuredConcurrency :> es
     , Concurrent :> es
     )
  => TimeoutConf
  -> Int
  -> Int
  -> Eff es [Reachability Impure]
startLobby timeoutConf = startLobby' timeoutConf []
