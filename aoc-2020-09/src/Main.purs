module Main where

import Prelude

import Data.Array (any, foldMap, mapMaybe)
import Data.BigInt as BigInt
import Data.CatQueue as CatQueue
import Data.Foldable (for_, maximum, minimum, traverse_)
import Data.List.Lazy as L
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala, alaF)
import Data.Pair (Pair(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Input (readStdin)
import Pipes (await, yield, (>->))
import Pipes as Pipes
import Pipes.Core (Pipe)
import Pipes.Prelude as P

chunks :: forall a m r. Ord a => Monad m => Int -> Pipe a (Tuple (Set a) a) m r
chunks n = (CatQueue.fromFoldable <$> L.replicateM n await) >>= go
  where
    go buffer = do
      item <- await
      yield (Tuple (Set.fromFoldable buffer) item)
      let buffer' = CatQueue.snoc buffer item
      case CatQueue.uncons buffer' of
        Nothing -> go buffer'
        Just (Tuple _ b) -> go b

findConsecutive :: forall a m r. Ring a => Ord a => Monad m => a -> Pipe a (Pair a) m r
findConsecutive n = go { sum: zero, buffer: CatQueue.empty }
  where
    snoc :: a -> { sum :: a, buffer :: CatQueue.CatQueue a } -> { sum :: a, buffer :: CatQueue.CatQueue a }
    snoc r state = { sum: state.sum + r, buffer: CatQueue.snoc state.buffer r }
    discardLeft state = case CatQueue.uncons state.buffer of
      Nothing -> state
      Just (Tuple l buffer') -> { sum: state.sum - l, buffer: buffer' }
    go :: { sum :: a, buffer :: CatQueue.CatQueue a } -> _
    go state = do
      item <- await
      reduceState (snoc item state)
    reduceState :: { sum :: a, buffer :: CatQueue.CatQueue a } -> _
    reduceState state = case compare state.sum n of
      LT -> go state
      GT -> reduceState (discardLeft state)
      EQ -> do
        traverse_ yield $ Pair <$> (minimum state.buffer) <*> (maximum state.buffer)
        reduceState (discardLeft state)

isInvalid :: forall a. Ring a => Ord a => Tuple (Set a) a -> Boolean
isInvalid (Tuple s n) = not (any go s)
  where
    go m = m + m /= n && Set.member (n - m) s

defaultInput :: String
defaultInput = """35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"""

main :: Effect Unit
main = launchAff_ do
  Tuple n inputText <- maybe (Tuple 5 defaultInput) (Tuple 25) <$> readStdin
  let input = mapMaybe BigInt.fromString $ split (Pattern "\n") inputText
  result <- P.head (
    Pipes.each input >->
    chunks n >->
    P.filter isInvalid >->
    P.map snd
  )
  traverse_ (log <<< BigInt.toString) result
  for_ result \target -> do
    maybe2 <- P.head (Pipes.each input >-> findConsecutive target)
    for_ maybe2 \result2 -> do
      logShow result2
      log <<< BigInt.toString <<< ala Additive foldMap $ result2



