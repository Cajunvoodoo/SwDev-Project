{-# LANGUAGE NoOverloadedLists #-}
module Main where

import Bazaar.Common hiding (toList)
import Bazaar.Player.Mechanism
import Bazaar.Player.Player
import Data.Default
import Bazaar.State.GameState
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List as List
import Effectful
import Data.JsonStream.Parser
import System.Environment (getArgs)
import Bazaar.Server.Server (runBazaarSeededWithBonus)
import Effectful.Dispatch.Dynamic

-- | The types of bonuses supported by XServer.
data Bonus
  = RWB -- ^ Red White and Blue. +10 points.
  | SEY -- ^ All colors of Seychelle's flag. +50 points.
 deriving (Show)

$( deriveJSON
    defaultOptions
      { sumEncoding = UntaggedValue
      }
    ''Bonus
 )

-- | Convert a 'Bonus' to a function that can be used by 'interpose'.
bonusToFunc
  :: forall p es a
   . (PlayerInteraction p :> es)
  => Maybe Bonus
  -> (Eff es a -> Eff es a)
bonusToFunc = \case
  Just RWB -> bonusInterposeHelper rwbBonus
  Just SEY -> bonusInterposeHelper seyBonus
  Nothing -> id
 where
   rwbBonus PlayerState {..}
     | [RedPbl, WhitePbl, BluePbl] `isSubset` concatPs cards =
       PlayerState {points = points + 10, ..}
     | otherwise = PlayerState {..}
   seyBonus PlayerState {..}
     | [BluePbl, YellowPbl, RedPbl, WhitePbl, GreenPbl] `isSubset` concatPs cards =
       PlayerState {points = points + 50, ..}
     | otherwise = PlayerState {..}
   bonusInterposeHelper :: (PlayerState -> PlayerState) -> (Eff es a -> Eff es a)
   bonusInterposeHelper bonusPred = interpose_ \case
     MarkTurn gs -> send (MarkTurn @p gs)
     KickPlayer pv rp -> send (KickPlayer @p pv rp)
     SetupPlayer eqnT rp -> send (SetupPlayer @p eqnT rp)
     AskPebbleOrTrades ts -> send (AskPebbleOrTrades @p ts)
     AskForCardBuy ts -> send (AskForCardBuy @p ts)
     Win rp gs -> send (Win @p rp gs)
     GetWinnersAndKicked gs@GameState{..} -> do
       let gs' = gs {players = fmap (modifyRpState bonusPred) players}
       send (GetWinnersAndKicked @p gs')

main :: IO ()
main = do
  let timeoutConf = def {gameRecvTimeout = 4000}
  port <- parseArgs
  (gs, BonusFunc bonusFunc) <- parseStdin
  (sort -> winners, sort -> kicked) <-
    runBazaarSeededWithBonus timeoutConf port bonusFunc gs
  BS.putStrLn $ encode winners
  BS.putStrLn $ encode kicked

-- | Helper for readability, but exists purely to wrap the bonus function creation
-- to satisfy the typechecker.
data BonusFunc where
  BonusFunc
    :: (forall es p a. (PlayerInteraction p :> es) => Eff es a -> Eff es a)
    -> BonusFunc

-- | Parse the data sent on stdin as configuration.
parseStdin :: IO (GameState Void, BonusFunc)
parseStdin = do
  streamParseJsonOptional 2 1 BS.getLine isEOF >>= \case
    Left err -> error err
    Right ([toEqnT -> eqnTRes, toGs -> gsRes], [toBonus -> bonus]) ->
      case (,) <$> eqnTRes <*> gsRes of
        Error err -> error err
        Success (eqnT, gsNoEqns) -> do
          let gs = gsNoEqns {equations = eqnT}
          pure (gs, BonusFunc $ bonusToFunc bonus)
    Right _ -> error "too few or too many arguments on stdin for XServer"

-- | Parse a JSON Value to an EquationTable.
toEqnT :: Value -> Result EquationTable
toEqnT = fromJSON @EquationTable

-- | Parse a JSON Value to an GameState.
toGs :: Value -> Result (GameState Void)
toGs = fromJSON @(GameState Void)

-- | Parse a JSON Value to a Maybe Bonus.
toBonus :: Maybe Value -> Maybe Bonus
toBonus = flatten . fmap (fromJSON @Bonus)
 where
  flatten :: Maybe (Result a) -> Maybe a
  flatten Nothing = Nothing
  flatten (Just (Error _)) = Nothing
  flatten (Just (Success v)) = Just v

-- | Stream-parse N JSON Values using the provided functions to obtain input and
-- check EOF.
streamParseJsonOptional
  :: (Monad m)
  => Int -- ^ Number of required arguments
  -> Int -- ^ Number of optional arguments
  -> m StrictByteString
  -- ^ Function to read input (usually a single line)
  -> m Bool
  -- ^ function to check if end of field has been reached
  -> m (Either String ([Value], [Maybe Value]))
streamParseJsonOptional numElems numOptional readInp isEOF = do
  let
    loop parseOutput parsedElems parsedOptionals = do
      case parseOutput of
        ParseYield val cont -> do
          if numElems == length parsedElems
            then loop cont parsedElems (parsedOptionals ++ [Just val])
            else loop cont (parsedElems ++ [val]) parsedOptionals
        ParseNeedData cont -> do
          eof <- isEOF
          case (eof, length parsedElems == numElems) of
            (_, True) ->
              let remainingOptionals = numOptional - length parsedOptionals
                  fullParsedOptionals =
                    parsedOptionals <> replicate remainingOptionals Nothing
              in pure $ Right (parsedElems, fullParsedOptionals)
            (False, _)
              | length parsedElems < numElems -> do
                  content <- readInp
                  loop (cont content) parsedElems parsedOptionals
            _ -> pure $ Left "Premature end of input or too many elements"
        ParseFailed err -> do
          pure $ Left err
        ParseDone unconsumed -> do
          loop (runParser' @Value value unconsumed) parsedElems parsedOptionals
  loop
    (runParser @Value value)
    ([] :: [Value])
    ([] :: [Maybe Value])

-- | Parse the port argument.
parseArgs :: IO Int
parseArgs = do
  [port] <- getArgs
  pure (read port)
