{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Bazaar.Common hiding (toList)
import Bazaar.Player.Mechanism
import Bazaar.Player.Reachability
import Data.Aeson
import Data.ByteString qualified as BS
import qualified Ki.Unlifted as Ki
import Bazaar.Player.Strategy
import Bazaar.Client.Referee
import qualified Data.Text.Encoding as T
import System.Environment (getArgs)
import GHC.Conc
import Bazaar.Client.Client

-- consumes ActorsB
-- | Parse the port argument.
parseArgs :: IO Int
parseArgs = do
  [port] <- getArgs
  pure (read port)

main :: IO ()
main = do
  port <- parseArgs
  streamParseJson 1 BS.getLine isEOF >>= \case
    Left err -> fail err
    Right [toActors -> actorsB] -> do
      case actorsB of
        Error err -> fail err
        Success actors -> do
          threadAliveTags <- newTVarIO (replicate (length actors) ())
          forM_ actors \actor -> do
            let name = getName actor
            let strategy = SomeStrategy $ policyToStrategy $ getActorPurchasePolicy actor
            let client = mkLocalClient (T.encodeUtf8 name) strategy port actor
            threadDelay delayBetweenForks
            void $ forkIO $ do
              void $ runClient client
              atomically $ do
                threadTags <- readTVar threadAliveTags
                writeTVar threadAliveTags (drop 1 threadTags)
          atomically do
           threadTags <- readTVar threadAliveTags
           when (null threadTags) (pure ())
           retry

toActors :: Value -> Result [Actor]
toActors = fromJSON @[Actor]

-- | Spawn each remote client. Used by 'runRemoteGameWithActors'.
spawnClients
  :: Int -> Ki.Scope -> [(Actor, SomeStrategy)] -> IO [Ki.Thread (Actor, WinResult)]
spawnClients port scope actors = do
  forM actors \(actor, strat) -> do
    let name = getName actor
    let client = mkLocalClient (T.encodeUtf8 name) strat port actor
    threadDelay delayBetweenForks
    Ki.fork scope ((,) actor <$> runClient client)

-- | Delay between spawning the server and the clients and between spawning
-- each individual client. Prevents race conditions on who connects first.
delayBetweenForks :: Int
delayBetweenForks = 10000
