module Main where

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, gets, modify_, evalState)
import Control.Monad.Trans.Class (lift)
import Data.Array (uncons)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Pair as Pair
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Input (intLines, readStdin)
import Prelude (Unit, bind, flip, discard, const, map, pure, show, ($), (*), (-), (<$>), (<<<), (>>=))

checkPair :: Int -> Int -> ExceptT (Pair Int) (State (Set.Set Int)) Unit
checkPair target x = do
  let match = target - x
  lift (gets (Set.member match)) >>= if _
    then throwError (Pair x match)
    else lift $ modify_ (Set.insert x)

findPair :: Int -> Array Int -> Maybe (Pair Int)
findPair target xs = either pure (const Nothing) <<< flip evalState Set.empty <<< runExceptT $ traverse (checkPair target) xs

mulPair :: Pair Int -> Int
mulPair = Pair.uncurry (*)

mulTriple :: Tuple Int (Pair Int) -> Int
mulTriple = Tuple.uncurry (*) <<< map mulPair

findTriple :: Int -> Array Int -> Maybe (Tuple Int (Pair Int))
findTriple target xs = do
  ht <- uncons xs
  (Tuple ht.head <$> findPair (target - ht.head) ht.tail) <|> findTriple target ht.tail

-- Day 1 part 1
getPart1Result :: Maybe String -> Maybe Int
getPart1Result input =
  input >>= intLines >>= findPair 2020 >>= pure <<< mulPair

-- Day 1 part 2
getPart2Result :: Maybe String -> Maybe Int
getPart2Result input =
  input >>= intLines >>= findTriple 2020 >>= pure <<< mulTriple

main :: Effect Unit
main = launchAff_ do
  text <- readStdin
  log <<< show $ getPart1Result text
  log <<< show $ getPart2Result text
