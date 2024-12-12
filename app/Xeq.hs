module Main where

import Bazaar.Common.Bank
import Bazaar.Common.Equations
import Bazaar.Common.Hand
import Bazaar.Common.Internal.Prelude
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BS

main :: IO ()
main = do
  eqns <- xeq
  let foo = A.encode $ fmap A.toJSON eqns
  BS.putStrLn foo

-- | Run the testing task for milestone 3. Stream-parses JSON from stdin,
-- taking an EquationTable, a 'Hand', and a
xeq :: IO (Vector DirectionalEquation)
xeq = do
  streamParseJson 3 BS.getLine isEOF >>= \case
    Left fail ->
      error $ "failed to create 3-tuple of equations: " <> fail
    Right [eqnsV, walletV, bankV] -> do
      let eqnTRes = A.fromJSON @EquationTable eqnsV
          walletRes = A.fromJSON @Hand walletV
          bankRes = A.fromJSON @Bank bankV
      case (,,) <$> eqnTRes <*> walletRes <*> bankRes of
        A.Success (eqnT, wallet, bank) -> do
          let possibleEquation = filterTable eqnT bank wallet
          pure possibleEquation
        A.Error err -> fail err
    _ -> fail "Invalid 3-tuple"
