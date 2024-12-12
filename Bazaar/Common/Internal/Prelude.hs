module Bazaar.Common.Internal.Prelude
  ( module Control.Monad
  , module Control.Monad.Extra
  , module Data.Aeson
  , module Data.Bifunctor
  , module Data.ByteString
  , module Data.ByteString.Lazy
  , module Data.Coerce
  , module Data.Containers.ListUtils
  , module Data.Foldable
  , module Data.Foldable.Extra
  , module Data.Function
  , module Data.Map.Strict
  , module Data.Monoid
  , module Data.MultiSet
  , module Data.Proxy
  , module Data.Set
  , module Data.Text
  , module Data.These
  , module Data.Vector
  , module Deque.Strict
  , module Diagrams.Core.Types
  , module Diagrams
  , module GHC.Generics
  , module GHC.TypeLits
  , module Numeric.Natural
  , module Prelude
  , module System.IO
  , module System.Random
  , module Data.Void
  , allCombinations
  , hasAny
  , permuteWithRep
  , permuteWithoutRep
  , for
  , concatFor
  , tracePretty
  , maybeToError
  , tracePrettyS
  , traceAnnotated
  , remove
  , returnWhen
  , returnMaybe
  , toIntegral
  , returnMaybeM
  , nTimes
  , streamParseJson
  , contextualize
  , contextualizeWith
  , contextualizeExn
  , onEarlyReturn
  , EarlyReturn
  , returnEarly_
  , pattern List
  , pattern ListS
  , recvLatinText
  , timeout
  , race
  , openServerSocket
  , runNetworkRun
  , type NetworkRun
  , mkListOfN
  , writeStderr
  ) where

import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Extra (ifM, unlessM, whenM)
import Data.Aeson (FromJSON, ToJSON, Value, fromJSON, toJSON)
import Data.Bifunctor (first, second)
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Coerce (coerce)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (Foldable (..))
import Data.Foldable.Extra
import Data.Function
import Data.JsonStream.Parser (ParseOutput (..), runParser, runParser', value)
import Data.List (permutations, subsequences)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Monoid
import Data.MultiSet (MultiSet)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Text (Text)
import Data.These (These (..))
import Data.Vector (Vector)
import Data.Void (Void)
import Debug.Trace (trace)
import Deque.Strict (Deque)
import Diagrams
  ( HasOrigin
  , HasStyle
  , Juxtaposable
  , N
  , TrailLike
  , Transformable
  , V
  , V2
  )
import Diagrams.Core.Types (Diagram, QDiagram)
import Effectful
import Effectful.Error.Static
import Effectful.Error.Static qualified as Error
import Effectful.Exception qualified as Exception
import GHC.Generics
import GHC.TypeLits (KnownNat, Nat, natVal, SNat)
import Numeric.Natural
import System.IO (isEOF, stderr, stdin, stdout)
import System.Random (RandomGen)
import Text.Show.Pretty (ppShow)
import Prelude
import Effectful.Network (Network, Socket, AddrInfo)
import Effectful.Concurrent
import Data.Text.Encoding (decodeLatin1)
import qualified Effectful.Network as Network
import qualified Effectful.Ki as Ki
import Effectful.Concurrent.STM
import Effectful.Ki (StructuredConcurrency)
import Effectful.Dispatch.Static
import qualified Network.Run.TCP as Run
import qualified GHC.TypeNats as TypeNats
import qualified Data.Vector.Sized as Sized
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as BS
import Data.Aeson (decode)

-- | Compute all combinations of a given set of elements.
allCombinations :: [a] -> [[a]]
allCombinations [] = [[]]
allCombinations (x : xs) =
  allCombinations xs ++ fmap (x :) (allCombinations xs)

-- | Does the first list contain any elments with the second?
hasAny :: (Eq a) => [a] -> [a] -> Bool
hasAny [] _ = False
hasAny _ [] = False
hasAny search (x : xs) = (x `elem` search) || hasAny search xs

-- | Permute a list with repetitions.
--
-- Ex:
--
-- @
-- >>> permuteWithRep 2 [1,2,3]
-- [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,1],[2,1],[3,1],[2,2],[3,2],[3,3]]
-- @
permuteWithRep :: (Ord a) => Int -> [a] -> [[a]]
permuteWithRep count elems =
  nubOrd . fmap (take count) . subsequences . take cyclesNeeded $ cycle elems
 where
  cyclesNeeded = count * length elems

-- | Permute a list and all of its sublists without repetitions where
-- order matters.
--
-- Ex:
--
-- @
-- >>> import Data.List
-- >>> sortOn length $ permuteWithoutRep [1,2]
-- [[],[1],[2],[1,2],[2,1]]
-- @
permuteWithoutRep :: (Ord a) => [a] -> [[a]]
permuteWithoutRep elems = go (length elems) elems
 where
  -- permute lists by taking N elements and permuting them and their children
  go 0 = (: [])
  go count =
    nubOrd -- nlogn
      . permuteFewer
      . permutations
   where
    permuteFewer perms =
      perms ++ concatMap (go (count - 1) . take (count - 1)) perms

-- | Flipped 'fmap'.
for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

-- | Flipped 'concatMap'.
concatFor :: (Foldable t) => t a -> (a -> [b]) -> [b]
concatFor = flip concatMap

-- | Like 'traceShowId', but with pretty-printing.
tracePretty :: (Show a) => a -> a
tracePretty a = trace (ppShow a) a

-- | Like 'trace', but with pretty-printing.
tracePrettyS :: (Show a) => a -> b -> b
tracePrettyS a = trace (ppShow a)

-- | Like 'tracePretty', but with an annotation at the beginning.
traceAnnotated :: (Show a) => String -> a -> a
traceAnnotated prompt a = trace (show prompt <> ": " <> ppShow a) a

-- | Convert a 'Maybe' value to a function in the 'Error' effect.
maybeToError :: (Error e :> es, Show e) => e -> Maybe a -> Eff es a
maybeToError e = \case
  Nothing -> throwError e
  Just a -> pure a

-- | Remove all elements in the second collection from the first list.
remove :: (Foldable t, Eq a) => [a] -> t a -> [a]
remove = foldl' (flip List.delete)

-- | Throw an 'Error' @e@ when 'False'.
returnWhen :: (Error e :> es, Show e) => e -> Bool -> Eff es ()
returnWhen e p
  | p = Error.throwError e
  | otherwise = pure ()

-- | Throw an 'Error' @e@ when 'Nothing'.
returnMaybe :: (Error e :> es) => e -> Maybe a -> Eff es a
returnMaybe e = \case
  Nothing -> Error.throwError_ e
  Just a -> pure a

-- | Throw an 'Error' @e@ when the action results in 'Nothing'.
returnMaybeM :: (Error e :> es) => e -> Eff es (Maybe a) -> Eff es a
returnMaybeM e ma =
  ma >>= \case
    Nothing -> Error.throwError_ e
    Just a -> pure a

-- | Convenient wrapper for 'fromIntegral' with type arguments flipped.
toIntegral :: forall b a. (Integral a, Integral b) => a -> b
toIntegral = fromIntegral @a @b

-- | Apply a function @n@ times to a given value.
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

-- | Stream-parse N JSON Values using the provided functions to obtain input and
-- check EOF.
streamParseJson
  :: (Monad m)
  => Int
  -> m StrictByteString
  -- ^ Function to read input (usually a single line)
  -> m Bool
  -- ^ function to check if end of field has been reached
  -> m (Either String [Value])
streamParseJson numElems readInp isEOF = do
  let
    loop parseOutput parsedElems = do
      case parseOutput of
        ParseYield val cont -> do
          loop cont (parsedElems ++ [val])
        ParseNeedData cont -> do
          eof <- isEOF
          case (eof, length parsedElems == numElems) of
            (_, True) -> pure $ Right parsedElems
            (False, _)
              | length parsedElems < numElems -> do
                  content <- readInp
                  loop (cont content) parsedElems
            _ -> pure $ Left "Premature end of input or too many elements"
        ParseFailed err -> do
          pure $ Left err
        ParseDone unconsumed -> do
          loop (runParser' @Value value unconsumed) parsedElems
  loop
    (runParser @Value value)
    ([] :: [Value])

-- | Contextualize an 'Error' with new information.
contextualize
  :: (Error e' :> es, Show e')
  => (e -> e')
  -> Eff (Error e : es) a
  -> Eff es a
contextualize f eff = do
  eErr <- Error.runErrorNoCallStack eff
  case eErr of
    Left e -> Error.throwError (f e)
    Right a -> pure a

-- | Like 'contextualize', but for 'Exception's.
contextualizeExn
  :: (Error e' :> es, Show e')
  => (SomeException -> e')
  -> Eff es a
  -> Eff es a
contextualizeExn f eff = do
  eErr <- Exception.trySync eff
  case eErr of
    Left e -> Error.throwError (f e)
    Right a -> pure a

-- | Like 'contextualize', but specialized to tuples.
contextualizeWith
  :: (Error (e, t) :> es, Show e, Show t) => t -> Eff (Error e : es) a -> Eff es a
contextualizeWith a = contextualize (,a)

-- | Type synonym for 'Error'.
type EarlyReturn = Error.Error

-- | Synonym for 'runErrorNoCallStackWith'. Rolls better off the tongue.
onEarlyReturn :: (e -> Eff es a) -> Eff (EarlyReturn e : es) a -> Eff es a
onEarlyReturn = Error.runErrorNoCallStackWith

-- | Synonym for 'throwError_'.
returnEarly_ :: (Error e :> es) => e -> Eff es a
returnEarly_ = Error.throwError_

-- | Pattern for lists.
pattern List :: forall a. [a] -> [a]
pattern List as = as

-- | Pattern for the singleton list.
pattern ListS :: forall a. a -> [a]
pattern ListS a = [a]

-- | Receive @sz@ bytes of Latin1 text from the socket.
recvLatinText :: (Network :> es) => Socket -> Int -> Eff es (Maybe Text)
recvLatinText conn sz = decode @Text . BS.fromStrict <$> Network.recv conn sz

-- | Wait at most @time@ microseconds for the action to finish.
timeout :: (StructuredConcurrency :> es, Concurrent :> es) => Int -> Eff es () -> Eff es ()
timeout time = race (threadDelay time)

-- | Perform two actions concurrently, and when the first action terminates, stop
-- executing the other.
race :: (StructuredConcurrency :> es, Concurrent :> es) => Eff es a -> Eff es a -> Eff es a
race action1 action2 =
  Ki.scoped \scope -> do
    resultVar <- Ki.newEmptyTMVarIO
    _ <- Ki.fork scope (action1 >>= \res -> atomically (putTMVar resultVar res))
    _ <- Ki.fork scope (action2 >>= \res -> atomically (putTMVar resultVar res))
    atomically $ takeTMVar resultVar

-- | The NetworkRun effect. Wrapper for the functions in the "Network.Run.TCP"
-- module.
data NetworkRun :: Effect

type instance DispatchOf NetworkRun = 'Static 'WithSideEffects
newtype instance StaticRep NetworkRun = Unit ()

-- | Run the 'NetworkRun' effect.
runNetworkRun :: (IOE :> es) => Eff (NetworkRun : es) a -> Eff es a
runNetworkRun = evalStaticRep (Unit ())

-- | Wraps 'S.openServerSocket'.
openServerSocket :: (NetworkRun :> es) => AddrInfo -> Eff es Socket
openServerSocket = unsafeEff_ . Run.openServerSocket
{-# INLINE openServerSocket #-}

mkListOfN :: Natural -> (b -> (a, b)) -> b -> [a]
mkListOfN n mkFun g =
  TypeNats.withSomeSNat n \(snat :: SNat n) -> TypeNats.withKnownNat snat $
    Sized.toList (Sized.unfoldrN @n mkFun g)
{-# INLINE mkListOfN #-}

writeStderr :: (IOE :> es) => Text -> Eff es ()
writeStderr text = do
  liftIO $ T.hPutStr stderr text
{-# INLINE writeStderr #-}
