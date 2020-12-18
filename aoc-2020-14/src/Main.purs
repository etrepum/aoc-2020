module Main where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.MonadZero (guard)
import Data.Array (fold, foldM, foldMap, foldl, fromFoldable, index, range)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (either)
import Data.Foldable (sum, traverse_)
import Data.FoldableWithIndex (foldMapWithIndex, foldWithIndexM)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Input (readStdin)
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.CodeUnits (eof, regex, string)
import Text.Parsing.StringParser.Combinators (choice, sepEndBy)

type Mask = { andMask :: BigInt, orMask :: BigInt }
data Instruction
  = SetMask Mask
  | SetMem { index :: BigInt, value :: BigInt }
derive instance genericInstruction :: Generic Instruction _
instance showInstruction :: Show Instruction where
  show = case _ of
    SetMask m -> "mask = " <> maskToString m
    SetMem { index, value } -> "mem[" <> show index <> "] = " <> BigInt.toString value

type ProgramState = { memory :: Map BigInt BigInt, mask :: Mask }

-- (andMask = 1, orMask = 0) is floating
-- (andMask = 0, orMask = 1) is on
-- (andMask = 0, orMask = 0) is off
unfloatAt :: Number -> Mask -> Array Mask
unfloatAt n m =
  if testBit m.andMask n
  then let andMask = setBitOff m.andMask n in
    [ { andMask, orMask: setBitOff m.orMask n }
    , { andMask, orMask: setBitOn m.orMask n }
    ]
  else if testBit m.orMask n
  then [m]
  else [m { andMask = setBitOn m.andMask n }]

unfloatMasks :: Mask -> Array Mask
unfloatMasks m = foldM (flip unfloatAt) m $ map Int.toNumber (range 0 35)

emptyMaskString :: String
emptyMaskString = foldMap (const "X") (range 0 35)

initialProgramState :: ProgramState
initialProgramState = { memory: Map.empty, mask: maskFromString emptyMaskString }

applyMask :: BigInt -> Mask -> BigInt
applyMask n { andMask, orMask } = BigInt.or (BigInt.and n andMask) orMask

setBitOff :: BigInt -> Number -> BigInt
setBitOff b n = BigInt.and (BigInt.not (BigInt.shl one n)) b

setBitOn :: BigInt -> Number -> BigInt
setBitOn b n = BigInt.or (BigInt.shl one n) b

testBit :: BigInt -> Number -> Boolean
testBit b n = BigInt.and one (BigInt.shr b n) == one

maskToString :: Mask -> String
maskToString { andMask, orMask } = foldMapWithIndex go (range 0 35)
  where
    go i _ = let bit = 35.0 - Int.toNumber i in if testBit orMask bit then "1" else if testBit andMask bit then "X" else "0"

maskFromString :: String -> Mask
maskFromString = foldl go { andMask: zero, orMask: zero } <<< toCharArray
  where
    step { andMask, orMask } = { andMask: BigInt.shl andMask one, orMask: BigInt.shl orMask one }
    go acc = let acc' = step acc in case _ of
      '0' -> acc'
      '1' -> acc' { orMask = BigInt.or acc'.orMask one }
      'X' -> acc' { andMask = BigInt.or acc'.andMask one }
      _ -> acc

bigIntParser :: Parser BigInt
bigIntParser =
  regex """\d+""" >>= maybe (fail "Invalid BigInt") pure <<< BigInt.fromString

instructionParser :: Parser Instruction
instructionParser = choice [ maskParser, memParser ]
  where
    maskParser = SetMask <<< maskFromString <$> (string "mask = " *> regex "[X10]{36}")
    memParser = do
      index <- string "mem[" *> bigIntParser
      value <- string "] = " *> bigIntParser
      pure $ SetMem { index, value }

inputParser :: Parser (Array Instruction)
inputParser = fromFoldable <$> sepEndBy instructionParser (regex """\r?\n""") <* eof

stepInstruction :: ProgramState -> Instruction -> ProgramState
stepInstruction s = case _ of
  SetMask m -> s { mask = m }
  SetMem { index, value } -> s { memory = Map.insert index (applyMask value s.mask) s.memory }

stepInstructionTwo :: ProgramState -> Instruction -> ProgramState
stepInstructionTwo s = case _ of
  SetMask m -> s { mask = m }
  SetMem { index: idx, value } ->
    s { memory = foldl (\ memory mask -> Map.insert (applyMask idx mask) value memory) s.memory (unfloatMasks s.mask) }

indexes :: BigInt -> Mask -> Array BigInt
indexes idx = map (applyMask idx) <<< unfloatMasks

memorySum :: ProgramState -> BigInt
memorySum s = sum s.memory

defaultInput :: String
-- defaultInput = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
-- mem[8] = 11
-- mem[7] = 101
-- mem[8] = 0"""
defaultInput = """mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"""


main :: Effect Unit
main = launchAff_ do
  inputText <- fromMaybe defaultInput <$> readStdin
  input <- either (liftEffect <<< throw <<< show) pure $ runParser inputParser inputText
  log ""
  log <<< BigInt.toString <<< memorySum $ foldl stepInstruction initialProgramState input
  log ""
  log <<< BigInt.toString <<< memorySum $ foldl stepInstructionTwo initialProgramState input
