-- | Remote Proxy for the client to talk.
module Bazaar.Client.Referee where

import Bazaar.Common
import Bazaar.Common.RuleBook
import Bazaar.Player.Mechanism
import Bazaar.Player.Strategy
import Bazaar.State.TurnState
import Data.Aeson
import Data.ByteString qualified as BS
import Effectful
import Effectful.Dispatch.Dynamic hiding (send)
import Effectful.Exception
import Effectful.Network
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.TH
import Network.Socket (defaultHints)
import qualified Effectful.State.Static.Shared as State
import Data.JsonStream.Parser
import UnliftIO.Retry ( fullJitterBackoff, applyAndDelay )
import Control.Retry (defaultRetryStatus)

-- | Configuration of a Client, which is used to determine the ip/port of the
-- server and the strategy of this client.
data ClientConfig p = ClientConfig
  { serverPort :: !Int
  -- ^ The port the server lives on. This type is given as an Int purely for
  -- compatibility with @network@ and its API.
  , serverHostName :: !HostName
  -- ^ 'HostName' or IP Address of the remote server.
  , strategy :: !SomeStrategy
  -- ^ The strategy this client will use.
  , rules :: !PureRules
  -- ^ Rules of the game the client will be playing in. This is shared knowledge.
  , clientName :: !StrictByteString
  -- ^ Client name. Must be valid according to the regex @^[a-zA-Z0-9]{1,20}$@,
  -- otherwise the client will be refused.
  , actorBehavior :: !Actor
  }

-- | Make a client for use in a testing apparatus. Connects to localhost with
-- the provided name, strategy, and server port.
mkLocalClient :: StrictByteString -> SomeStrategy -> Int -> Actor -> ClientConfig p
mkLocalClient name strat serverPort actor =
  ClientConfig
    { serverPort = serverPort
    , serverHostName = "127.0.0.1"
    , strategy = strat
    , rules = defRules
    , clientName = name
    , actorBehavior = actor
    }
 where
  -- | Default rules used by the clients. Used as an example. Rules are established
  -- similarly to pre-shared secrets in cryptography; our protocol does not handle
  -- rules transfer, so it must be performed (correctly) out-of-band.
  defRules :: PureRules
  defRules = defaultPureRules 4 20


-- | Request for a 'Pebble' instead of 'Equation's. This is used instead of
-- RequestAPebble in order to more closely match the spec.
data RequestPebble = RequestPebble

-- | The results of a client's game. Isomorphic to booleans.
data WinResult
  = Winner
  | Loser
  deriving stock (Show)

-- | Helper to convert Bools to WinResults; they are trivially isomorphic.
boolToWinRes :: Bool -> WinResult
boolToWinRes False = Loser
boolToWinRes True = Winner

-- | Rpcs that can be received by the client.
data ClientRpc
  = -- | An Rpc failed to decode. The error is attached.
    DecodeError !String
  | -- | The game is over, and our result is attached. This is distinct from
    -- 'ClientRpc' because it deserves special care and attention; once we receive
    -- a 'GameOver', we must break from whatever it is we are doing and disconnect.
    GameOver !WinResult
  | -- | A protocol-level Rpc. This will never contain an RpcWin; see 'GameOver'
    -- for that case. Contains the Rpc the server sent to this client.
    ClientRpc !(Rpc Void)
  deriving stock (Show)

-- | Remote proxy-like effect to talk to the Referee from the Client.
data RefereeInteraction :: Effect where
  -- | Send our name to the referee over the socket.
  SendName :: Socket -> StrictByteString -> RefereeInteraction m ()
  -- | Send our EitherPebbleOrTrades to the referee over the socket.
  SendPebbleOrTrades
    :: Socket -> PTAction -> RefereeInteraction m ()
  -- | Send our CardBuy to the referee over the socket.
  SendCardBuy :: Socket -> CardBuy -> RefereeInteraction m ()
  -- | Receive a 'ClientRpc' from the referee. This is a blocking function.
  RecvRpc :: Socket -> RefereeInteraction m ClientRpc

makeEffect ''RefereeInteraction

-- | Make the error used by the recv* family of functions.
mkRecvError :: (Show a, HasCallStack) => String -> a -> b
mkRecvError expected rpc =
  error $ "Unexpected JSON in " <> expected <> ": " <> show rpc

-- | Receive the 'RpcSetup' rpc (blocking).
-- Errors if any other Rpc is received, and returns early if a 'GameOver' is received.
recvSetup
  :: (RefereeInteraction :> es, EarlyReturn WinResult :> es, Reader Socket :> es, HasCallStack)
  => Eff es EquationTable
recvSetup = do
  sock <- Reader.ask
  recvRpc sock >>= \case
    ClientRpc (RpcSetup eqnT) -> pure eqnT
    GameOver win -> returnEarly_ win
    rpc -> mkRecvError "recvSetup" rpc

-- | Receive the 'RpcRPOT' rpc (blocking).
-- Errors if any other Rpc is received, and returns early if a 'GameOver' is received.
recvRequestForTrades
  :: (RefereeInteraction :> es, EarlyReturn WinResult :> es, Reader Socket :> es, HasCallStack)
  => Eff es (TurnState Void)
recvRequestForTrades = do
  sock <- Reader.ask
  recvRpc sock >>= \case
    ClientRpc (RpcRPOT ts) -> pure ts
    GameOver win -> returnEarly_ win
    rpc -> mkRecvError "recvRequestForTrades" rpc

-- | Receive the 'RpcRFCB' rpc (blocking).
-- Errors if any other Rpc is received, and returns early if a 'GameOver' is received.
recvRequestForCardBuy
  :: (RefereeInteraction :> es, EarlyReturn WinResult :> es, Reader Socket :> es, HasCallStack)
  => Eff es (TurnState Void)
recvRequestForCardBuy = do
  sock <- Reader.ask
  recvRpc sock >>= \case
    ClientRpc (RpcRFCB ts) -> pure ts
    GameOver win -> returnEarly_ win
    rpc -> mkRecvError "recvRequestForCardBuy" rpc

-- | Create a TCP client, closing it after the callback returns.
-- Applies [FullJitterBackoff](https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/)
-- if the connection cannot be established. Each backoff performed results in a
-- @.@ printing to 'stderr'.
runTCPClient
  :: (IOE :> es, Network :> es) => HostName -> ServiceName -> (Socket -> Eff es a) -> Eff es a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  bracket
    (open addr) -- (resource creation)
    close       -- (resource cleanup)
    client      -- (action)
 where
  -- retreives the flags, family, socket type, protocol, address, canonical name
  resolve = do
    let hints = defaultHints {addrSocketType = Stream}
    head <$> getAddrInfo (Just hints) (Just host) (Just port)
  -- Open the socket, catching errors if they happen and cleaning up if so.
  open addr = bracketOnError (openSocket addr) close $ \sock -> do
    establishConn sock addr

  -- connect to the socket with backoff used by AWS's cloud services:
  -- https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
  establishConn sock addr = do
    let applyBackoff = applyAndDelay (fullJitterBackoff (millisToMicros 100))
        mkConn = do
          connect sock $ addrAddress addr
          pure $ Just sock
        loop status = do
          -- catch and ignore the exception this function throws
          -- It only throws exceptions when connection fails
          mbConn <- mkConn `catch` (\(_ :: IOException) -> pure Nothing)
          case mbConn of
            Just conn -> pure conn
            Nothing -> do
              writeStderr "."
              retryStatus <- applyBackoff status
              case retryStatus of
                Nothing -> error "Backoff ended: connection establishment timed out"
                Just status' -> loop status'
    loop defaultRetryStatus

-- | Handler for the 'RefereeInteraction' effect. Uses network sockets to talk to
-- the remote referee.
runRefereeInteraction
  :: (Network :> es, HasCallStack)
  => Eff (RefereeInteraction : es) a
  -> Eff es a
runRefereeInteraction = reinterpret_ internalEffs \case
  SendName sock name -> sendAll sock ("\"" <> name <> "\"")
  SendPebbleOrTrades sock pebOrTrade -> do
    let jsonVal = case pebOrTrade of
          RequestAPebble -> Bool False
          Exchange trades -> toJSON trades
    sendEncoded sock jsonVal
  SendCardBuy sock cb -> do
    sendEncoded sock (toJSON cb)
  RecvRpc sock -> do
    -- We must use stream parsing here, as adjacent JSON values can sit next
    -- to each other in the buffer. There are two solutions:
    -- 1. Lazily read from the socket buffer one byte at a time until a full,
    --    JSON object can be parsed. This is inefficient, complex, and slow, but
    --    conceptually the simplest.
    -- 2. Read strictly, append to parse-buffer, and parse a single JSON object
    --    from that buffer. This requires a bit of State, but requires fewer
    --    moving parts (regarding the socket calls).
    -- We conceptually choose the second option (maintaining parse state instead).
    streamParseStatefully sock >>= \case
      Right (RpcWin iWon) -> pure . GameOver $ boolToWinRes iWon
      Right rpc -> pure $ ClientRpc rpc
      Left err -> pure $ DecodeError err
 where
   internalEffs =
     State.evalState (runParser @(Rpc Void) value)

-- | Helper for sending a JSON value of the socket.
sendEncoded :: (Network :> es) => Socket -> Value -> Eff es ()
sendEncoded sock val = void $ send sock $ BS.toStrict $ encode val

-- | Parse an incoming packet using the current parse state. Parse state is
-- maintained between calls. It is necessary to stream parse because multiple RPCs
-- may be sitting in the buffer (e.g., if the game started in a state that is over,
-- then the referee will likely very quickly send two RPCs). It is rare, but it
-- can happen, and stream parsing is the most robust mechanism to handle it.
streamParseStatefully
  :: forall p es
   . ( Network :> es
     , HasCallStack
     , State.State (ParseOutput (Rpc p)) :> es
     , FromJSON (TurnState p)
     )
  => Socket
  -> Eff es (Either String (Rpc p))
streamParseStatefully sock = do
  let loop parseState = do
        case parseState of
          ParseYield val cont -> do
            State.put cont
            pure $ Right val
          ParseNeedData cont -> do
            newInput <- recv sock maxRecvSize
            case newInput of
              "" -> -- Socket EOF indication
                pure $ Left "EOF in socket: expected more data, but socket is empty"
              inp -> do
                loop (cont inp)
          ParseFailed err -> do
            pure $ Left err
          ParseDone unconsumed -> do
            loop (runParser' @(Rpc p) value unconsumed)
  parseState <- State.get
  loop parseState
