module Main where

import Bazaar.Common hiding (toList)
import Options.Applicative
import System.Environment (getArgs, getExecutablePath)
import System.FilePath
import System.Posix.Process (executeFile)

argsShow :: Parser Bool
argsShow = switch (long "show" <> help "Show gtk window")

main :: IO ()
main = do
  (splitFileName -> (currentDir, _)) <- getExecutablePath
  rawArgs <- getArgs
  showWindow <- execParser $ info argsShow mempty
  when showWindow do
    executeFile (currentDir </> "xgames-dynamic") False rawArgs Nothing
  executeFile (currentDir </> "xgames-static") False rawArgs Nothing
