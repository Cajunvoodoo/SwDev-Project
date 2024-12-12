{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}

module Bazaar.Player.Mechanism where

import Bazaar.Common hiding (All)
import Bazaar.Common.RuleBook
import Bazaar.Player.CardMaximizer (maxCardBuyKMStrategy)
import Bazaar.Player.KMStrategy (KMStrategy)
import Bazaar.Player.Player
import Bazaar.Player.PointMaximizer (maxPointBuyKMStrategy)
import Bazaar.Player.Reachability as Reachability
import Bazaar.Player.Strategy
import Bazaar.State.GameState
import Bazaar.State.TurnState
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.MultiSet qualified as MS
import Deque.Strict qualified as Deque
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Ki (StructuredConcurrency)
import Effectful.Network hiding (send)
import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Effectful.TH
import System.Random qualified as Random
import Text.RE.TDFA
import Text.Show.Pretty (ppShow)
import Data.Default

-- NOTE: this is a temporary home for this code TODO
----------------------------
-- Connection Abstraction --
----------------------------

-- | Effect for communicating with a 'Reachability' (usually 'Impure').
data Connection p :: Effect where
  ReadPacket
    :: (Eq (Reachability p), FromJSON r)
    => Proxy r
    -> Reachability p
    -> Connection p m (Maybe r)
  SendPacket
    :: (Eq (Reachability p), ToJSON r)
    => r
    -> Reachability p
    -> Connection p m ()
  CloseConnection :: (Eq (Reachability p)) => Reachability p -> Connection p m ()

makeEffect_ ''Connection

-- | Read the oldest packet from the queue for the provided reachability, decoding
-- via its 'FromJSON' instance. This may block indefinitely.
readPacket
  :: forall r p es
   . (Eq (Reachability p), Connection p :> es, FromJSON r)
  => Proxy r
  -> Reachability p
  -> Eff es (Maybe r)

-- | Send a packet using the provided 'Reachability', encoding it to JSON first.
sendPacket
  :: (Eq (Reachability p), Connection p :> es, ToJSON r)
  => r
  -> Reachability p
  -> Eff es ()
-- Close a particular 'Reachability's connection, killing the associated worker,
-- if it exists.
closeConnection
  :: (Eq (Reachability p), Connection p :> es) => Reachability p -> Eff es ()

-- | Maximum number of bytes to receive at any given time.
maxRecvSize :: (Num a) => a
maxRecvSize = 40960

runConnectionImpure
  :: (Network :> es, NetworkRun :> es)
  => Eff (Connection Impure : es) a
  -> Eff es a
runConnectionImpure = interpret_ \case
  ReadPacket (_ :: Proxy r) (ImpureComm sock _) -> do
    rawPacket <-
      BS.fromStrict <$> recv sock maxRecvSize
    case eitherDecode @r rawPacket of
      Right res -> pure $ Just res
      Left err -> error $ "Failed to decode: " <> err
  SendPacket packet (ImpureComm sock _) -> do
    sendAll sock (BS.toStrict $ encode packet)
  CloseConnection (ImpureComm sock _) -> do
    shutdown sock ShutdownBoth

---------------------------------------------------------------------------------

-- * PlayerInteraction Effect

---------------------------------------------------------------------------------

-- | The PlayerInteraction effect enables the communication between the Referee and
-- the Players.
--
-- It is best practice to avoid instantiating @p@ until as late as possible,
-- ideally as late as the effect handler. Not only does this make code more
-- reusable, it makes it more /testable/. Instead of inspecting an the Reachability
-- value by instantiating @p@, it should be used as an argument to one of the
-- effectful functions, where the type of @a@ will be known statically.
data PlayerInteraction p :: Effect where
  MarkTurn :: GameState p -> PlayerInteraction p m ()
  KickPlayer :: PlayerViolation -> RemotePlayer p -> PlayerInteraction p m ()
  SetupPlayer :: EquationTable -> RemotePlayer p -> PlayerInteraction p m ()
  AskPebbleOrTrades :: TurnState p -> PlayerInteraction p m (Maybe PTAction)
  AskForCardBuy :: TurnState p -> PlayerInteraction p m (Maybe CardBuy)
  Win :: RemotePlayer p -> Bool -> PlayerInteraction p m ()
  GetWinnersAndKicked
    :: GameState p -> PlayerInteraction p m ([RemotePlayer p], [RemotePlayer p])

makeEffect_ ''PlayerInteraction

-- | Used as a hook for imposed effects. Does nothing by default.
markTurn :: (PlayerInteraction p :> es) => GameState p -> Eff es ()

-- | Kick the provided 'RemotePlayer', cleaning up any resources the player used.
kickPlayer
  :: (PlayerInteraction p :> es) => PlayerViolation -> RemotePlayer p -> Eff es ()

-- | Hand the 'RemotePlayer' an 'EquationTable'. This should only be called
-- once at the beginning of the game.
setupPlayer
  :: (PlayerInteraction p :> es) => EquationTable -> RemotePlayer p -> Eff es ()

-- | Give the player whose turn it is the turn information, returning their
-- request for either a 'Pebble' or a specific set of 'Trades'.
--
-- If a Player takes too long to respond, 'Nothing' is returned. What constitutes
-- "too long" is determined by the interpretation of the effect.
askPebbleOrTrades
  :: (PlayerInteraction p :> es) => TurnState p -> Eff es (Maybe PTAction)

-- | Give the player whose turn it is the turn information, returning their
-- desired set of 'Card's to buy.
--
-- If a Player takes too long to respond, 'Nothing' is returned. What constitutes
-- "too long" is determined by the interpretation of the effect.
askForCardBuy
  :: (PlayerInteraction p :> es) => TurnState p -> Eff es (Maybe CardBuy)

-- | Inform the player whether they have won.
win :: (PlayerInteraction p :> es) => RemotePlayer p -> Bool -> Eff es ()

-- | Get the winning players and the kicked players.
getWinnersAndKicked
  :: (PlayerInteraction p :> es)
  => GameState p
  -> Eff es ([RemotePlayer p], [RemotePlayer p])

-- | Kick the provided players and remove them from the 'GameState'.
kickPlayers
  :: ( PlayerInteraction p :> es
     , Eq (Reachability p)
     , Foldable f
     , HasName (Reachability p)
     )
  => GameState p
  -> f (PlayerViolation, RemotePlayer p)
  -> Eff es (GameState p)
kickPlayers = do
  foldlM
    ( \gs' (v, rp) -> do
        kickPlayer v rp
        pure $ gs' {players = Deque.filter ((/=) (getName rp) . getName) (players gs')}
    )

---------------------------------------------------------------------------------

-- * Effect Handlers

---------------------------------------------------------------------------------

-- | Run the 'PlayerInteraction' effect purely using the provided 'EquationTable'
-- and list of players. Only the names of the 'RemotePlayer's are used.
runPurePlayerInteraction
  :: forall a es
   . (Rules Pure :> es)
  => EquationTable
  -> Deque (RemotePlayer Pure, SomeStrategy)
  -> Eff (PlayerInteraction Pure : es) a
  -> Eff es a
{- FOURMOLU_DISABLE -}
runPurePlayerInteraction eqnT remotePlayers = reinterpret internalEffs \_ -> \case
  MarkTurn _gs                 -> pure ()
  GetWinnersAndKicked gs       -> getWinnersAndKicked gs
  KickPlayer _violation player -> do
    State.modify (player :)
  SetupPlayer _eqnT _player    -> pure ()
  AskPebbleOrTrades ts         -> Just <$> askPebbleOrTrades ts
  AskForCardBuy ts             -> Just <$> askForCardBuy ts
  Win _player _isWinner        -> pure ()
 where
  internalEffs =
    State.evalState (Nothing :: Maybe (StrategyResult, Hand))
      . State.evalState (mempty :: [RemotePlayer Pure])

  getWinnersAndKicked gs = do
    kickedPlayers <- State.get
    let getPoints = points . playerState
    let maxPoints = maximum $ fmap getPoints (players gs)
        winningPlayers =
          toList $ Deque.filter ((==) maxPoints . getPoints) (players gs)
    pure (winningPlayers, kickedPlayers)

  askPebbleOrTrades
    :: forall es
     . (Rules Pure :> es, State (Maybe (StrategyResult, Hand)) :> es)
    => TurnState Pure
    -> Eff es PTAction
  askPebbleOrTrades ts@TurnState{..} = do
    pr <- getPureRules
    let rps = toList remotePlayers
        rpConns = fmap (first conn) rps
    let stratF = case lookup (conn activePlayer) rpConns of
          Nothing -> error "the 'impossible' happened! strategy doesn't exist!"
          Just s -> s
    let stratRes = runStrategy pr stratF eqnT ts
    State.put $ Just (stratRes, hand $ playerState activePlayer)
    let ptAction = case stratRes of
          NeedPebble _ -> RequestAPebble
          Decided (WithExchangeTurnActions {taExchanges}) -> Exchange taExchanges
    pure ptAction

  askForCardBuy
    :: forall es
     . (Rules Pure :> es, State (Maybe (StrategyResult, Hand)) :> es)
    => TurnState Pure
    -> Eff es CardBuy
  askForCardBuy ts@TurnState{..} = do
    prevStratRes <- State.get
    let cards = case prevStratRes of
          Nothing -> error $ concat @[]
            [ "the 'impossible' happened! "
            , "askPebbleOrTrades not called before askForCardBuy!\n"
            , "Turn State:\n"
            , ppShow ts
            ]
          Just (NeedPebble k, prevHand) ->
            let newHand = hand $ playerState activePlayer
                newPeb =
                  head
                  . MS.toList
                  . convert @(MultiSet Pebble)
                  $ newHand `difference` prevHand
             in taCardBuy $ k newPeb
          Just (Decided actions, _) -> taCardBuy actions
    pure cards
{- FOURMOLU_ENABLE -}

-- TODO: move this definition
data Rpc p
  = RpcSetup EquationTable
  | RpcWin Bool
  | -- | Request Pebble or Trades
    RpcRPOT (TurnState p)
  | -- | Request For Card Buy
    RpcRFCB (TurnState p)

deriving instance (Show (Reachability p)) => Show (Rpc p)
deriving instance (Eq (Reachability p)) => Eq (Rpc p)

instance (ToJSON (TurnState p)) => ToJSON (Rpc p) where
  -- TODO: how do we format these damn things? list of list? or flat list?
  toJSON (RpcSetup eqnT) = toJSON ("setup" :: Text, eqnT)
  toJSON (RpcWin win) = toJSON ("win" :: Text, ListS win)
  toJSON (RpcRPOT ts) = toJSON ("request-pebble-or-trades" :: Text, ListS ts)
  toJSON (RpcRFCB ts) = toJSON ("request-cards" :: Text, ListS ts)

instance (FromJSON (TurnState p)) => FromJSON (Rpc p) where
  parseJSON = \case
    Array ["setup", eqnT] -> RpcSetup <$> parseJSON eqnT
    Array ["win", Array [win]] -> RpcWin <$> parseJSON win
    Array ["request-pebble-or-trades", Array [ts]] -> RpcRPOT <$> parseJSON ts
    Array ["request-cards", Array [ts]] -> RpcRFCB <$> parseJSON ts
    _ -> fail "Could not parse Rpc"

-- | Newtype for the PebbleOrTrade response, which--in the schema--is terribly
-- designed for anyone but a lisper (perhaps we should practice good API design :)
-- Includes a 'FromJSON' instance to deserialize.
newtype RpcRespPebbleOrTrade = RpcRespPOT (Maybe [DirectionalEquation])
  deriving (Show)

instance FromJSON RpcRespPebbleOrTrade where
  parseJSON (Bool False) = pure $ RpcRespPOT Nothing
  parseJSON a = RpcRespPOT . Just <$> parseJSON @[DirectionalEquation] a

-- | Timeout configuration used in 'runImpurePlayerInteraction' and downstream
-- functions. All times are specified in milliseconds. Use 'millisToMicros' to
-- convert these times to those used by 'threadDelay' and 'timeout'.
data TimeoutConf = TimeoutConf
  { lobbyFillTimeout :: Int
  , nameRecvTimeout :: Int
  , gameRecvTimeout :: Int
  }

instance Default TimeoutConf where
  def :: TimeoutConf
  def = TimeoutConf
    { lobbyFillTimeout = 20000
    , nameRecvTimeout = 2000
    , gameRecvTimeout = 20000
    }

-- | Convert Milliseconds to μs.
millisToMicros :: Int -> Int
millisToMicros = (* 1000)

-- | Run the 'PlayerInteraction' effect impurely over the network. Uses a timeout
-- of 20s for player responses.
runImpurePlayerInteraction
  :: forall es a
   . (Connection Impure :> es, Concurrent :> es, StructuredConcurrency :> es)
  => TimeoutConf
  -> Eff (PlayerInteraction Impure : es) a
  -> Eff es a
runImpurePlayerInteraction toc = reinterpret_ internalEffs \case
  MarkTurn _gs -> pure ()
  GetWinnersAndKicked gs -> getWinnersAndKicked gs
  KickPlayer _violation player -> do
    State.modify (player :)
  SetupPlayer eqnT (conn -> player) -> do
    sendPacket (RpcSetup eqnT) player
  Win (conn -> player) isWinner -> do
    sendPacket (RpcWin isWinner) player
  AskPebbleOrTrades ts -> do
    -- :TypeInferenceInWhereBoundEffectfulFunctions
    -- For some reason, type inference fails when 'toc' is not passed as an
    -- argument; using it via RecordWildCards results in very odd type errors due
    -- to poor type inference. Thus, we simply opt to pass as an argument and use
    -- the record selectors.
    resp <- askPebbleOrTrades toc ts
    -- Just Trades OR False/RequestAPebble. Anything else = fail
    case resp of
      Just (RpcRespPOT (Just es)) -> pure . Just $ Exchange es
      Just (RpcRespPOT Nothing) -> pure . Just $ RequestAPebble
      _ -> pure Nothing -- Failed!
  AskForCardBuy ts ->
    -- See :TypeInferenceInWhereBoundEffectfulFunctions
    askForCardBuy toc ts
 where
  internalEffs =
    State.evalState (mempty :: [RemotePlayer Impure])

  getWinnersAndKicked gs = do
    kickedPlayers <- State.get
    let getPoints = points . playerState
    let maxPoints = maximum $ fmap getPoints (players gs)
        winningPlayers =
          toList $ Deque.filter ((==) maxPoints . traceAnnotated "points" . getPoints) (traceAnnotated "players" $ players gs)
    pure (winningPlayers, kickedPlayers)

  askPebbleOrTrades toc ts@TurnState {activePlayer} = do
    let conn' = conn activePlayer
    sendPacket (RpcRPOT ts) conn'
    resp <- newTVarIO Nothing
    timeout (millisToMicros $ gameRecvTimeout toc) do
      packet <- readPacket (Proxy @RpcRespPebbleOrTrade) conn'
      atomically $ writeTVar resp packet
      pure ()
    readTVarIO resp
  askForCardBuy toc ts@TurnState {activePlayer} = do
    let conn' = conn activePlayer
    sendPacket (RpcRFCB ts) conn'
    resp <- newTVarIO Nothing
    timeout (millisToMicros $ gameRecvTimeout toc) do
      packet <- readPacket (Proxy @CardBuy) conn'
      atomically $ writeTVar resp packet
      pure ()
    readTVarIO resp

-- | 'interpose' exceptions onto player actions via a function to determine if a
-- particular player ought to throw an exception. The exceptions are thrown
-- *before* the action is sent to the original handler.
addExnsPlayerInteraction
  :: forall p es a
   . (PlayerInteraction p :> es, HasName (Reachability p))
  => (forall m b. (Monad m) => Text -> PlayerInteraction p m b -> m ())
  -> Eff es a
  -> Eff es a
addExnsPlayerInteraction doExn =
  interpose_ \case
    MarkTurn gs -> do
      send (MarkTurn @p gs)
    GetWinnersAndKicked gs -> do
      send (GetWinnersAndKicked @p gs)
    KickPlayer violation player -> do
      send (KickPlayer @p violation player)
    SetupPlayer eqnT player -> do
      let name = getName player
      doExn name (SetupPlayer @p eqnT player)
      send (SetupPlayer @p eqnT player)
    AskPebbleOrTrades ts@TurnState {activePlayer} -> do
      let name = getName activePlayer
      _ <- doExn name (AskPebbleOrTrades @p ts)
      send (AskPebbleOrTrades @p ts)
    AskForCardBuy ts@TurnState {activePlayer} -> do
      let name = getName activePlayer
      _ <- doExn name (AskForCardBuy @p ts)
      send (AskForCardBuy @p ts)
    Win player isWinner -> do
      let name = getName player
      _ <- doExn name (Win @p player isWinner)
      send (Win @p player isWinner)

addCheatingPlayerInteraction
  :: forall p es a
   . (PlayerInteraction p :> es, HasName (Reachability p))
  => EquationTable
  -> [Actor]
  -> Eff es a
  -> Eff es a
addCheatingPlayerInteraction eqnT actors = interpose_ \case
  MarkTurn gs -> do
    send (MarkTurn @p gs)
  Win player isWinner -> do
    send (Win @p player isWinner)
  GetWinnersAndKicked gs -> do
    send (GetWinnersAndKicked @p gs)
  KickPlayer violation player -> do
    send (KickPlayer @p violation player)
  SetupPlayer eqnT player -> do
    send (SetupPlayer @p eqnT player)
  AskPebbleOrTrades ts@TurnState {activePlayer} -> do
    if cheaterPresent activePlayer actors
      then do
        let cheater = getCheat activePlayer actors
        case calculatePebbleOrTradeWithCheats cheater eqnT ts of
          Nothing -> send (AskPebbleOrTrades @p ts)
          res -> pure res
      else send (AskPebbleOrTrades @p ts)
  AskForCardBuy ts@TurnState {activePlayer} -> do
    if cheaterPresent activePlayer actors
      then do
        let cheater = getCheat activePlayer actors
        case calculateCardBuyWithCheats cheater ts of
          Nothing -> send (AskForCardBuy @p ts)
          res -> pure res
      else send (AskForCardBuy @p ts)

mkRandom
  :: forall a. (a -> Bool) -> (forall g. (RandomGen g) => g -> (a, g)) -> a
mkRandom isDup mkA = go 0
  where
  go n
    | isDup (fst $ mkA (Random.mkStdGen n)) = go (n + 1)
    | otherwise = fst $ mkA (Random.mkStdGen n)

cheaterPresent
  :: (HasName (Reachability p)) => RemotePlayer p -> [Actor] -> Bool
cheaterPresent player actors =
  getName player `elem` fmap getName (findCheaters actors)

findCheaters :: [Actor] -> [Actor]
findCheaters = filter isCheater
 where
  isCheater (WithCheatActor {}) = True
  isCheater _ = False

getCheat
  :: (HasName (Reachability p)) => RemotePlayer p -> [Actor] -> ActorCheat
getCheat player actors =
  let foundCheaters = filter ((==) (getName player) . getName) actors
   in getActorCheat $ head foundCheaters

getActorCheat :: Actor -> ActorCheat
getActorCheat (WithCheatActor _ _ cheat) = cheat
getActorCheat _ = error "Actor does not have cheats"

data Actor
  = NoExnActor Text PurchasePolicy
  | WithExnActor Text PurchasePolicy ActorExn
  | WithCheatActor Text PurchasePolicy ActorCheat
  | WithLoopActor Text PurchasePolicy ActorExn Integer
  deriving stock (Show)

instance ToJSON Actor where
  toJSON (NoExnActor name pp) = toJSON (name, pp)
  toJSON (WithExnActor name pp exn) = toJSON (name, pp, exn)
  toJSON (WithCheatActor name pp cheat) = toJSON (name, pp, "a cheat" :: Text, cheat)
  toJSON (WithLoopActor name pp exn count) = toJSON (name, pp, exn, count)
instance HasName Actor where
  getName (NoExnActor name _) = name
  getName (WithExnActor name _ _) = name
  getName (WithCheatActor name _ _) = name
  getName (WithLoopActor name _ _ _) = name

instance ToJSON ActorExn where
  toJSON = \case
    ExnSetup -> "setup"
    ExnAskPebbleEqn -> "request-pebble-or-trades"
    ExnAskForCards -> "request-cards"
    ExnWin -> "win"

-- TODO: order generator shit logically, and break up into modules
instance ToJSON PurchasePolicy where
  toJSON = \case
    PurchasePoints -> String "purchase-points"
    PurchaseSize -> "purchase-size"

instance FromJSON Actor where
  parseJSON = withArray "Actor" \case
    [String name, ppStr] -> do
      matchName name
      pp <- parseJSON ppStr
      pure $ NoExnActor name pp
    [String name, ppStr, exnStr] -> do
      matchName name
      pp <- parseJSON ppStr
      exn <- parseJSON exnStr
      pure $ WithExnActor name pp exn
    [String name, ppStr, "a cheat", cheatStr] -> do
      matchName name
      pp <- parseJSON ppStr
      cheat <- parseJSON cheatStr
      pure $ WithCheatActor name pp cheat
    [String name, ppStr, exnStr, countStr] -> do
      matchName name
      pp <- parseJSON ppStr
      exn <- parseJSON exnStr
      count <- parseJSON countStr
      pure $ WithLoopActor name pp exn count
    _ -> fail "Too few or too many elements"
   where
    matchName name
      | matched (name ?=~ [re|^[a-zA-Z0-9]{1,20}$|]) = pure ()
      | otherwise =
          fail $
            "Name " <> show name <> " is too long, empty, or contains special characters"

policyToStrategy :: PurchasePolicy -> KMStrategy
policyToStrategy = \case
  PurchasePoints -> maxPointBuyKMStrategy
  PurchaseSize -> maxCardBuyKMStrategy

data PurchasePolicy
  = PurchasePoints
  | PurchaseSize
  deriving (Eq, Show)

instance FromJSON PurchasePolicy where
  parseJSON = withText "PurchasePolicy" \case
    "purchase-points" -> pure PurchasePoints
    "purchase-size" -> pure PurchaseSize
    s -> fail $ "invalid string for PurchasePolicy: " <> show s

data ActorCheat
  = CheatUseNonExistEqn -- RPOT
  | CheatBankCannotTrade -- RPOT
  | CheatWalletCannotTrade -- RPOT
  | CheatBuyNonExistCard -- RFCB
  | CheatWalletCannotBuyCard -- RFCB
  deriving stock (Show)

instance FromJSON ActorCheat where
  parseJSON = withText "ActorCheat" \case
    "use-non-existent-equation" -> pure CheatUseNonExistEqn
    "bank-cannot-trade" -> pure CheatBankCannotTrade
    "wallet-cannot-trade" -> pure CheatWalletCannotTrade
    "buy-unavailable-card" -> pure CheatBuyNonExistCard
    "wallet-cannot-buy-card" -> pure CheatWalletCannotBuyCard
    _ -> fail "Cheat does not exist"

data ActorExn
  = ExnSetup
  | ExnAskPebbleEqn
  | ExnAskForCards
  | ExnWin
  deriving stock (Show, Eq, Ord)

instance FromJSON ActorExn where
  parseJSON = withText "ActorExn" \case
    "setup" -> pure ExnSetup
    "request-pebble-or-trades" -> pure ExnAskPebbleEqn
    "request-cards" -> pure ExnAskForCards
    "win" -> pure ExnWin
    f -> fail $ "Invalid exn function: " <> show f

addLoopingPlayerInteraction
  :: forall p es a
   . ( PlayerInteraction p :> es
     , HasName (Reachability p)
     , StructuredConcurrency :> es
     , Concurrent :> es
     )
  => TimeoutConf
  -> [Actor]
  -> Eff es a
  -> Eff es a
addLoopingPlayerInteraction toc actors = impose_ (State.evalState (initLoopPlayers :: Map (Text, ActorExn) Integer)) \case
  -- GOAL: when Int value for the player is 0, replace their strategy with an
  -- infinite loop strategy.
  MarkTurn gs -> do
    send (MarkTurn @p gs)
  GetWinnersAndKicked gs -> do
    send (GetWinnersAndKicked @p gs)
  KickPlayer violation player -> do
    send (KickPlayer @p violation player)
  Win player isWinner -> do
    -- :LoopingPlayerExceptionHack
    -- a bit of an ugly hack, we rely on the exceptions to kick the player.
    -- ideally, the api would be able to handle this, but it doesn't make sense
    -- for 'Win' or 'SetupPlayer' to return 'Maybe' (or anything except unit),
    -- as it is effectively fire-and-forget (there is no return in the protocol)
    -- See :DeepDarkErrorImplementationNote for a similar rant.
    -- See :TypeInferenceInWhereBoundEffectfulFunctions regarding 'toc' argument.
    Maybe.fromJust
      <$!> mkLoop toc ExnWin player (Just <$> send (Win @p player isWinner))
  SetupPlayer eqnT player -> do
    Maybe.fromJust
      <$!> mkLoop toc ExnSetup player (Just <$> send (SetupPlayer @p eqnT player))
  AskPebbleOrTrades ts -> do
    mkLoop toc ExnAskPebbleEqn (activePlayer ts) (send $ AskPebbleOrTrades @p ts)
  AskForCardBuy ts -> do
    mkLoop toc ExnAskForCards (activePlayer ts) (send $ AskForCardBuy @p ts)
 where
  initLoopPlayers =
    Map.fromList
      [((name, exn), count) | (WithLoopActor name _ exn count) <- actors]
  mkLoop
    :: forall a
     . (Show a)
    => TimeoutConf
    -> ActorExn
    -> RemotePlayer p
    -> Eff (State (Map (Text, ActorExn) Integer) : es) (Maybe a)
    -> Eff (State (Map (Text, ActorExn) Integer) : es) (Maybe a)
  mkLoop TimeoutConf{gameRecvTimeout} exn activePlayer sendAction = do
    loopPlayers <- State.get
    let name = getName activePlayer
    case Map.lookup (name, exn) loopPlayers of
      Nothing -> sendAction
      Just count -> do
        res <- newTVarIO Nothing
        timeout (millisToMicros gameRecvTimeout) do
          if count <= 1
            then bottom'
            else do
              sendActionRes <- sendAction
              atomically $ writeTVar res sendActionRes
        State.modify (Map.insert (name, exn) (count - 1))
        readTVarIO res -- if this is empty, we done messed up (fatal)

-- | Bottom type. Infinite loop.
bottom :: a
bottom = bottom
{-# NOINLINE bottom #-}

-- | Bottom, but for applicatives.
bottom' :: (Applicative f) => f a
bottom' = pure bottom
{-# NOINLINE bottom' #-}

getActorPurchasePolicy :: Actor -> PurchasePolicy
getActorPurchasePolicy (NoExnActor _ pp) = pp
getActorPurchasePolicy (WithExnActor _ pp _) = pp
getActorPurchasePolicy (WithCheatActor _ pp _) = pp
getActorPurchasePolicy (WithLoopActor _ pp _ _) = pp

instance ToJSON ActorCheat where
  toJSON = \case
    CheatUseNonExistEqn -> "use-non-existent-equation"
    CheatBankCannotTrade -> "bank-cannot-trade"
    CheatWalletCannotTrade -> "wallet-cannot-trade"
    CheatBuyNonExistCard -> "buy-unavailable-card"
    CheatWalletCannotBuyCard -> "wallet-cannot-buy-card"

-- TODO: document
calculatePebbleOrTradeWithCheats
  :: forall p
   . ActorCheat
  -> EquationTable
  -> TurnState p
  -> Maybe PTAction
calculatePebbleOrTradeWithCheats cheater eqnT ts@TurnState {bank} =
  case cheater of
    CheatUseNonExistEqn -> do
      let nonExistEqn = fst . splitEquation $ mkRandom (inTable eqnT) mkRandomEquation
      Just $ Exchange [nonExistEqn]
    CheatBankCannotTrade -> do
      cheatTrade
        ( \eqn ->
            not $
              satisfies (mkEqnHand eqn) bank eqn -- (demorgan's) bank satisfies
                && satisfies apHand (mkEqnHand eqn) eqn -- hand satisfies
        )
    CheatWalletCannotTrade -> do
      cheatTrade
        ( \eqn ->
            satisfies (mkEqnHand eqn) bank eqn -- bank can trade
              && not (satisfies apHand (mkEqnHand eqn) eqn) -- hand cannot trade
        )
    _ -> Nothing
 where
  apHand = hand $ getPlayerState ts
  mkEqnHand eqn = lhsToPebbleSet eqn <> rhsToPebbleSet eqn
  cheatTrade notSatisfy = do
    let eqns = toList $ mapDirectionally id eqnT
        unsatEqns = filter notSatisfy eqns
    case unsatEqns of
      [] -> Nothing
      (eqn : _) -> Just $ Exchange [eqn]

calculateCardBuyWithCheats
  :: forall p
   . ActorCheat
  -> TurnState p
  -> Maybe CardBuy
calculateCardBuyWithCheats cheater ts@TurnState {cardsInPlay} =
  case cheater of
    CheatBuyNonExistCard ->
      let nonExistCard = mkRandom (`elem` cardsInPlay) mkRandomCard
      in Just [nonExistCard]
    CheatWalletCannotBuyCard ->
      if hugeHand `isSubset` apHand
        then Nothing
        else do
          let cannotBuyCard =
                filter (not . (`isSubset` apHand)) cardsInPlay
          case cannotBuyCard of
            [] -> Nothing
            _ -> Just cannotBuyCard
    _ -> Nothing
 where
  hugeHand = concatMap (replicate 5) pebbles
  apHand = hand $ getPlayerState ts
