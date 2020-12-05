module Main where

import Prelude

import Control.Alt (alt)
import Data.Either (either, hush, isRight)
import Data.Int as Int
import Data.List (List, all, foldl)
import Data.Maybe (fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Foreign.Object as FO
import Input (readStdin)
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.CodeUnits (eof, regex, string)
import Text.Parsing.StringParser.Combinators (choice, sepEndBy, sepEndBy1)

requiredFields :: Array String
requiredFields =
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  ]

optionalFields :: Array String
optionalFields = ["cid"]

type Passport = FO.Object String

oneOfString :: Array String -> Parser String
oneOfString = choice <<< map string

fieldNameParser :: Parser String
fieldNameParser = oneOfString (requiredFields <> optionalFields)

fieldValueParser :: Parser String
fieldValueParser = regex """\S+"""

fieldParser :: Parser Passport
fieldParser =
  FO.singleton <$> fieldNameParser <*> (string ":" *> fieldValueParser)

passportParser :: Parser Passport
passportParser =
  FO.unions <$> sepEndBy1 fieldParser (regex """ +|\r?\n""")

inputParser :: Parser (List Passport)
inputParser = sepEndBy passportParser (regex """\r?\n""") <* eof

checkValidity :: Passport -> Boolean
checkValidity passport = all (flip FO.member passport) requiredFields

checkRange :: Int -> Int -> Int -> Boolean
checkRange lo hi x = x >= lo && x <= hi

validators :: Array (Tuple String (Parser Boolean))
validators =
  -- byr (Birth Year) - four digits; at least 1920 and at most 2002.
  [ Tuple "byr" (checkRange 1920 2002 <$> intParser)
  -- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  , Tuple "iyr" (checkRange 2010 2020 <$> intParser)
  -- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  , Tuple "eyr" (checkRange 2020 2030 <$> intParser)
  -- hgt (Height) - a number followed by either cm or in:
  -- If cm, the number must be at least 150 and at most 193.
  -- If in, the number must be at least 59 and at most 76.
  , Tuple "hgt" do
      n <- intParser
      alt 
        (string "cm" *> pure (checkRange 150 193 n))
        (string "in" *> pure (checkRange 59 76 n))
  -- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  , Tuple "hcl" (regex """#[0-9a-f]{6}""" *> pure true)
  -- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  , Tuple "ecl" (oneOfString ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] *> pure true)
  -- pid (Passport ID) - a nine-digit number, including leading zeroes.
  , Tuple "pid" (regex """[0-9]{9}""" *> pure true)
  -- cid (Country ID) - ignored, missing or not.
  ]

checkFullValidity :: Passport -> Boolean
checkFullValidity passport = all checkValidator validators
  where
    checkValidator :: Tuple String (Parser Boolean) -> Boolean
    checkValidator (Tuple k parser) = fromMaybe false $
      FO.lookup k passport >>= (hush <<< runParser (parser <* eof))

countBy :: forall a. (a -> Boolean) -> List a -> Int
countBy f = foldl (\ acc x -> acc + if f x then 1 else 0) 0

intParser :: Parser Int
intParser =
  regex """\d+""" >>= maybe (fail "Invalid int") pure <<< Int.fromString


defaultInput :: String
defaultInput = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"""

main :: Effect Unit
main = launchAff_ do
  inputText <- fromMaybe defaultInput <$> readStdin
  input <- either (liftEffect <<< throw <<< show) pure $ runParser inputParser inputText
  log <<< show $ countBy checkValidity input
  log <<< show $ countBy checkFullValidity input
