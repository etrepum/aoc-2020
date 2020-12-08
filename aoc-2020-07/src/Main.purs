module Main where

import Prelude

import Data.Either (either)
import Data.Enum (fromEnum)
import Data.Foldable (class Foldable, any, foldMap)
import Data.FoldableWithIndex (foldMapWithIndex, foldlWithIndex)
import Data.Function.Memoize (memoize)
import Data.Int as Int
import Data.Lazy (defer, force)
import Data.List (List, fold)
import Data.Map (Map, keys, lookup, member, singleton, unionWith)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (un)
import Data.Set (Set)
import Data.Traversable (class Traversable)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Input (readStdin)
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.CodeUnits (eof, regex, string)
import Text.Parsing.StringParser.Combinators (choice, sepBy, sepEndBy)

newtype MultiSet k v = MultiSet (Map k v)
derive newtype instance showMultiSet :: (Show k, Show v) => Show (MultiSet k v)
derive newtype instance monoidMultiSet :: (Ord k) => Monoid (MultiSet k v)
derive newtype instance foldableMultiSet :: (Ord k) => Foldable (MultiSet k)
derive newtype instance functorMultiSet :: (Ord k) => Functor (MultiSet k)
derive newtype instance traversableMultiSet :: (Ord k) => Traversable (MultiSet k)

instance semigroupMultiSet :: (Ord k, Semigroup v) => Semigroup (MultiSet k v) where
  append (MultiSet a) (MultiSet b) = MultiSet $ unionWith append a b

type Ruleset = Map String (MultiSet String (Additive Int))

intParser :: Parser Int
intParser =
  regex """\d+""" >>= maybe (fail "Invalid int") pure <<< Int.fromString

ruleParser :: Parser Ruleset
ruleParser = singleton <$> keyParser <*> valueParser
  where
    keyParser = bagParser <* string " bags contain "
    valueParser = choice
      [ pure mempty <* string "no other bags"
      , fold <$> sepBy containParser (string ", ")
      ] <* string "."
    bagParser = regex """\S+ \S+"""
    containParser = do
      n <- intParser <* string " "
      k <- bagParser <* string " bag"
      when (n > 1) (void (string "s"))
      pure (MultiSet (singleton k (Additive n)))

inputParser :: Parser Ruleset
inputParser = fold <$> sepEndBy ruleParser (regex """\r?\n""") <* eof

multiSetMember :: forall k v. Ord k => k -> MultiSet k v -> Boolean
multiSetMember k (MultiSet m) = member k m

multiSetKeys :: forall k v. Ord k => MultiSet k v -> Set k
multiSetKeys (MultiSet m) = keys m

countReachable :: String -> Ruleset -> Int
countReachable target rules = unwrap $ foldMap (Additive <<< fromEnum <<< isReachable) (keys rules)
  where
    unwrap (Additive n) = n
    isReachable = memoize \ k ->
      fromMaybe false $ any (\ sub -> sub == target || isReachable sub) <<< multiSetKeys <$> lookup k rules

countContains :: String -> Ruleset -> Int
countContains target rules = getCounts target
  where
    getCounts :: String -> Int
    getCounts = memoize $ \ k -> case lookup k rules of
      Nothing -> 0
      Just (MultiSet subs) -> foldlWithIndex (\ i acc (Additive a) -> acc + a + a * getCounts i) 0 subs

defaultInput :: String
defaultInput = """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""

main :: Effect Unit
main = launchAff_ do
  inputText <- fromMaybe defaultInput <$> readStdin
  input <- either (liftEffect <<< throw <<< show) pure $ runParser inputParser inputText
  log <<< show $ countReachable "shiny gold" input
  log <<< show $ countContains "shiny gold" input
