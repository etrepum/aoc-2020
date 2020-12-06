module Main where

import Prelude

import Data.Array (range)
import Data.Foldable (find, maximum, traverse_)
import Data.List (foldl)
import Data.Maybe (Maybe, fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Input (readStdin)

type Bound = { min :: Int, max :: Int }
type SeatBounds = { row :: Bound, col :: Bound }
type Seat = { row :: Int, col :: Int, id :: Int }

midpoint :: Bound -> Int
midpoint b = b.min + div (1 + b.max - b.min) 2

seatFold :: SeatBounds -> Char -> SeatBounds
seatFold s = case _ of
  'F' -> s { row = s.row { max = midpoint s.row } }
  'B' -> s { row = s.row { min = midpoint s.row } }
  'L' -> s { col = s.col { max = midpoint s.col } }
  'R' -> s { col = s.col { min = midpoint s.col } }
  _ -> s

bound :: Int -> Bound
bound max = { min: 0, max: max }

seatBounds :: { maxRow :: Int, maxCol :: Int } -> SeatBounds
seatBounds r = { row: bound r.maxRow, col: bound r.maxCol }

processLine :: { rows :: Int, cols :: Int } -> String -> Seat
processLine spec = toSeat <<< foldl seatFold (seatBounds { maxRow: spec.rows - 1, maxCol: spec.cols - 1 }) <<< toCharArray
  where
    toSeat :: SeatBounds -> Seat
    toSeat b = { row: b.row.min, col: b.col.min, id: b.col.min + b.row.min * spec.cols }

processSeats :: Array String -> Array Int
processSeats = map (_.id <<< processLine { rows: 128, cols: 8 })

gap :: Array Int -> Maybe Int
gap seats = do
  let s = Set.fromFoldable seats
  min <- Set.findMin s
  max <- Set.findMax s
  find (not <<< flip Set.member s) (range min max)

defaultInput :: String
defaultInput = """FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL"""

main :: Effect Unit
main = launchAff_ do
  seats <- processSeats <<< split (Pattern "\n") <<< fromMaybe defaultInput <$> readStdin
  traverse_ (log <<< show) (maximum seats)
  traverse_ (log <<< show) (gap seats)
