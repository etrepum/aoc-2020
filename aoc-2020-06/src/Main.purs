module Main where

import Prelude

import Data.Array.NonEmpty as NonEmpty
import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, ala, alaF)
import Data.Semigroup.Foldable (foldMap1)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Input (readStdin)

newtype SetIntersection a = SetIntersection (Set a)
derive instance newtypeSetIntersection :: Newtype (SetIntersection a) _
instance semigroupSetIntesection :: Ord a => Semigroup (SetIntersection a) where
  append (SetIntersection a) (SetIntersection b) = SetIntersection (Set.intersection a b)

parseGroups :: String -> Array (Array (Set Char))
parseGroups = map parseGroup <<< split (Pattern "\n\n")
  where
    parseGroup = map parseAnswer <<< split (Pattern "\n")
    parseAnswer = Set.fromFoldable <<< toCharArray

intersections :: Array (Set Char) -> Set Char
intersections = fromMaybe Set.empty <<< map (ala SetIntersection foldMap1) <<< NonEmpty.fromArray

defaultInput :: String
defaultInput = """abc

a
b
c

ab
ac

a
a
a
a

b"""

main :: Effect Unit
main = launchAff_ do
  groups <- parseGroups <<< fromMaybe defaultInput <$> readStdin
  log <<< show <<< alaF Additive foldMap (Set.size <<< Set.unions) $ groups
  log <<< show <<< alaF Additive foldMap (Set.size <<< intersections) $ groups
