module Main where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (filter, index)
import Data.Foldable (product)
import Data.Maybe (fromMaybe, maybe)
import Data.Number.Format (toString)
import Data.String as String
import Data.String.CodeUnits (charAt)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Input (readStdin)

lines :: String -> Array String
lines = filter (not <<< String.null) <<< String.split (String.Pattern "\n")

isTree :: String -> Int -> Boolean
isTree s n = maybe false (_ == '#') $ charAt (mod n (String.length s)) s

type TreeAcc =
  { col :: Int
  , row :: Int
  , trees :: Number
  }

countTrees :: { col :: Int, row :: Int } -> Array String -> Number
countTrees { col: colStride, row: rowStride } arr = tailRec go { col: 0, row: 0, trees: 0.0 }
  where
    go acc = maybe (Done acc.trees) Loop $ do
      line <- index arr acc.row
      pure
        { col: acc.col + colStride
        , row: acc.row + rowStride
        , trees: acc.trees + if isTree line acc.col then 1.0 else 0.0
        }

defaultText :: String
defaultText = """
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
"""

day2Strides :: Array { col :: Int, row :: Int }
day2Strides =
  [ { col: 1, row: 1 }
  , { col: 3, row: 1 }
  , { col: 5, row: 1 }
  , { col: 7, row: 1 }
  , { col: 1, row: 2 }
  ]

main :: Effect Unit
main = launchAff_ do
  inputLines <- lines <<< fromMaybe defaultText <$> readStdin
  log <<< toString $ countTrees { col: 3, row: 1 } inputLines
  let day2Results = map (flip countTrees inputLines) day2Strides 
  log <<< toString $ product day2Results
