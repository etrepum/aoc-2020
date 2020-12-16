module Main where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec, tailRecM)
import Control.Monad.State (State, execState, modify, runState)
import Data.Array (foldMap, index, mapWithIndex)
import Data.Foldable (sum, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Pair (Pair(..))
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import Input (readStdin)

type Floorplan = Array (Array Tile)
type Tile = Maybe Boolean

countOccupied :: Floorplan -> Int
countOccupied = flip execState 0 <<< go
  where
    go = traverse_ goRow
    goRow = traverse_ goTile
    goTile = traverse_ \x -> when x (void (modify (_ + 1)))

stepUntil :: Floorplan -> Int
stepUntil = tailRec go
  where
    go a = let b = stepPlan a in if a == b then Done (countOccupied a) else Loop b

logUntilDone :: forall m. MonadRec m => (Floorplan -> Tuple Floorplan Int) -> (Tuple Floorplan Int -> m Unit) -> Floorplan -> m Int
logUntilDone step logStep = tailRecM \f -> do
  let result = step f
  logStep result
  case result of
    Tuple f' 0 -> pure <<< Done $ countOccupied f'
    Tuple f' _ -> pure <<< Loop $ f'

stepPlan :: Floorplan -> Floorplan
stepPlan f = mapWithIndex goRow f
  where
    goRow i = mapWithIndex (goCol i)
    goCol i j = map (maybeSwap $ neighbors (Pair i j))
    maybeSwap x a = if a then x < 4 else x == 0
    neighbors coord =
      sum <<< map (occupied <<< addPair coord) $ directions
    occupied (Pair i j) = fromMaybe zero $ do
      row <- index f i
      col <- index row j
      map (if _ then one else zero) col

addPair :: forall a. Semiring a => Pair a -> Pair a -> Pair a
addPair (Pair a b) (Pair c d) = Pair (a + c) (b + d)

directions :: Array (Pair Int)
directions = do
  i <- [-1, 0, 1]
  Pair i <$> if i == 0 then [-1, 1] else [-1, 0, 1]

runPlanTwo :: Floorplan -> Floorplan
runPlanTwo = tailRec \f ->
  case stepPlanTwo f of
    Tuple f' 0 -> Done f'
    Tuple f' _ -> Loop f'

stepPlanTwo :: Floorplan -> Tuple Floorplan Int
stepPlanTwo f = runState (traverseWithIndex goRow f) 0
  where
    goRow :: Int -> Array Tile -> State Int (Array Tile)
    goRow i = traverseWithIndex (goCol i)
    goCol :: Int -> Int -> Tile -> State Int Tile
    goCol i j a = traverse (goCell (Pair i j)) a
    goCell :: Pair Int -> Boolean -> State Int Boolean
    goCell coord a = do
      let neighbors = countNeighbors coord
      if (a && neighbors > 4) || (not a && neighbors == 0)
      then modify (_ + 1) *> pure (not a)
      else pure a
    sightline :: Pair Int -> Pair Int -> Int
    sightline vec = tailRec \coord -> fromMaybe (Done zero) do
      let coord' = addPair coord vec
      status <- occupied coord'
      pure $ maybe (Loop coord') (if _ then Done one else Done zero) status
    countNeighbors :: Pair Int -> Int
    countNeighbors coord =
      sum $ map (flip sightline coord) directions
    occupied :: Pair Int -> Maybe Tile
    occupied (Pair i j) = do
      row <- index f i
      index row j
  
showRow :: Array Tile -> String
showRow = foldMap $ maybe "." (if _ then "#" else "L")

readRow :: String -> Array Tile
readRow = map go <<< toCharArray
  where
    go = case _ of
      'L' -> Just false
      '#' -> Just true
      _ -> Nothing

defaultInput :: String
defaultInput = """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""

logStep :: forall m. MonadEffect m => Tuple Floorplan Int -> m Unit
logStep (Tuple f n) = do
  log $ "Swaps: " <> show n
  log $ "Occupancy: " <> show (countOccupied f)
  traverse_ (log <<< showRow) f
  log ""

main :: Effect Unit
main = launchAff_ do
  inputText <- fromMaybe defaultInput <$> readStdin
  let input = map readRow $ split (Pattern "\n") inputText
  traverse_ (log <<< showRow) input
  log ""
  logShow (stepUntil input)
  log ""
  logUntilDone stepPlanTwo logStep input

