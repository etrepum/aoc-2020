module Main where

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Either (either, isRight)
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List)
import Data.Maybe (fromMaybe, maybe)
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf')
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Input (readStdin)
import Prelude (Unit, bind, discard, pure, show, unit, void, map, when, (==), (-), ($), (<$>), (*>), (<*), (+), (<), (<<<), (>), (>=), (>>=))
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.CodeUnits (anyChar, eof, regex, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (choice, sepEndBy)

type PasswordPolicy =
  { min :: Int
  , max :: Int
  , match :: String
  }

regexSpecial :: Regex
regexSpecial = unsafeRegex """[.*+\-?^${}()|[\]\\]""" RegexFlags.global

exactMatchRe :: String -> Parser String
exactMatchRe s = regex (replace regexSpecial "\\$&" s)

intParser :: Parser Int
intParser =
  regex """\d+""" >>= maybe (fail "Invalid int") pure <<< Int.fromString

lineParser :: Parser { policy :: PasswordPolicy, password :: String }
lineParser = do
  min <- intParser
  void (string "-")
  max <- intParser
  skipSpaces
  match <- regex """[^:]+"""
  void (string ":")
  skipSpaces
  password <- regex """\S+"""
  pure { policy: { min: min, max: max, match: match }, password: password }

inputParser :: Parser (List { policy :: PasswordPolicy, password :: String })
inputParser = sepEndBy lineParser (regex """\r?\n""") <* eof

part1Count :: List { policy :: PasswordPolicy, password :: String } -> Int
part1Count = sum <<< map \p -> if checkValidity p.policy p.password then 1 else 0

part2Count :: List { policy :: PasswordPolicy, password :: String } -> Int
part2Count = sum <<< map \p -> if go p.policy p.password then 1 else 0
  where
    go policy password = 
      let atIndex n = fromMaybe 0 do
            x <- indexOf' (Pattern policy.match) n password
            pure (if x == n then 1 else 0)
      in atIndex (policy.min - 1) + atIndex (policy.max - 1) == 1

policyParser :: PasswordPolicy -> Parser Unit
policyParser policy = do
  count <- parseUpToN (1 + policy.max)
  when (count < policy.min) (fail "Minimum unsatisfied")
  when (count > policy.max) (fail "Maximum unsatisfied")
  where
    matcher = exactMatchRe policy.match
    parseOnce = tailRecM parseOnceGo unit 
    parseOnceGo _ = choice
      [ matcher *> pure (Done true)
      , anyChar *> pure (Loop unit)
      , eof *> pure (Done false)
      ]
    parseUpToN target = tailRecM parseUpToNGo { target: target, matches: 0 }
    parseUpToNGo acc = if acc.matches >= acc.target
      then pure (Done acc.matches)
      else parseOnce >>= if _
        then pure (Loop (acc { matches = acc.matches + 1 }))
        else pure (Done acc.matches)

checkValidity :: PasswordPolicy -> String -> Boolean
checkValidity policy = isRight <<< runParser (policyParser policy)

main :: Effect Unit
main = launchAff_ do
  inputText <- fromMaybe "" <$> readStdin
  input <- either (liftEffect <<< throw <<< show) pure $ runParser inputParser inputText
  log $ show (part1Count input)
  log $ show (part2Count input)
