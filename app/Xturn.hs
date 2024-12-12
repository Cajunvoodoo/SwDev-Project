module Main where

import Bazaar.State.GameState
import Bazaar.Player.Reachability
import Bazaar.State.TurnState
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 qualified as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

main :: IO ()
main = do
  ts <- xturn
  let foo = A.encode ts
  BS.putStrLn foo

-- | Run the testing task for milestone 4. Stream-parses JSON from stdin,
-- taking an GameState
xturn :: IO (TurnState Pure)
xturn = do
  content <- LBS.getContents
  gameState <- A.throwDecode @(GameState Pure) content
  let extracted = extractTurnState gameState
  case extracted of
    Nothing -> error "Could not create a TurnState from provided GameState"
    Just ts -> pure ts
