module Main where

import Prelude

import Control.Comonad (extract)
import Control.Fold (Fold)
import Control.Fold as F
import Data.Array (mapMaybe, sort)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (traverse_)
import Data.Function.Memoize (memoize)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Pair (Pair(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Input (readStdin)

-- | A `Fold` which yields consecutive pairs
zeroPairs :: forall a. a -> Fold a (Pair a)
zeroPairs z = F.unfoldFold_ (Pair z z) step
  where
    step (Pair _ b) a = Pair b a

pipe :: forall a b c. Fold a b -> Fold b c -> Fold a c
pipe fa_ fb_ = F.unfoldFold (Tuple fa_ fb_) step (extract <<< snd)
  where
    step :: Tuple (Fold a b) (Fold b c) -> a -> Tuple (Fold a b) (Fold b c)
    step (Tuple fa fb) a =
      let fa' = F.stepFold a fa
      in Tuple fa' (F.stepFold (extract fa') fb)

overMaybe :: forall a b. Fold a b -> Fold (Maybe a) b
overMaybe f_ = F.unfoldFold f_ step extract
  where
    step :: Fold a b -> Maybe a -> Fold a b
    step f a = case a of
      Just a' -> F.stepFold a' f
      Nothing -> f

offByN :: forall a. Semiring a => Eq a => a -> Pair a -> a
offByN n (Pair a b) = if a + n == b then one else zero

countOffByN :: forall a. Semiring a => Eq a => a -> Fold (Pair a) a
countOffByN n = F.unfoldFold_ zero step
  where
    offBy = offByN n
    step f a = f + offBy a

partOne :: Fold Int Int
partOne = (*) <$> offBy 1 <*> (map (1 + _) (offBy 3))
  where
    offBy n = pipe (zeroPairs 0) (countOffByN n)

arrangements :: Set Int -> BigInt
arrangements plugs = maybe zero go $ Set.findMax plugs
  where
    go = memoize \n ->
      case compare n zero of
        LT -> zero
        EQ -> one
        GT -> if Set.member n plugs then recur n else zero
    recur n = go (n - 3) + go (n - 2) + go (n - 1)

defaultInput :: String
defaultInput = """28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"""

main :: Effect Unit
main = launchAff_ do
  Tuple n inputText <- maybe (Tuple 5 defaultInput) (Tuple 25) <$> readStdin
  let input = mapMaybe Int.fromString $ split (Pattern "\n") inputText
  let sortedInput = sort input
  let offBy1 = pipe (zeroPairs 0) (countOffByN 1)
  logShow (F.foldl partOne sortedInput)
  log <<< BigInt.toString <<< arrangements $ Set.fromFoldable sortedInput
