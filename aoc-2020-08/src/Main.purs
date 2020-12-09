module Main where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.MonadZero (guard)
import Data.Array (foldMap, fromFoldable, index, length, mapMaybe, range, updateAt)
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe.First (First(..))
import Data.Newtype (alaF)
import Data.Set (Set, empty, insert, member)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Exception (throw)
import Input (readStdin)
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.CodeUnits (eof, regex, string)
import Text.Parsing.StringParser.Combinators (choice, sepEndBy)

data Op
  = Nop
  | Acc
  | Jmp
derive instance genericOp :: Generic Op _

instance showOp :: Show Op where
  show = genericShow

type Instruction = { op :: Op, arg :: Int }
type Processor = { ip :: Int, acc :: Int }
type ExecState = { proc :: Processor, seen :: Set Int }

initialState :: ExecState
initialState = { proc: { ip: 0, acc: 0 }, seen: empty }

runUntilLoop :: Array Instruction -> Int
runUntilLoop instructions = tailRec go initialState
  where
    go { proc: p, seen: s } = maybe (Done p.acc) Loop do
      let ip = p.ip
      guard (not $ member ip s)
      p' <- step p <$> index instructions ip
      pure { proc: p', seen: insert ip s }

runOrFail :: Array Instruction -> Maybe Int
runOrFail instructions = tailRec go initialState
  where
    go { proc: p, seen: s } = 
      if member p.ip s then Done Nothing
      else case index instructions p.ip of
        Just ins -> Loop { proc: step p ins, seen: insert p.ip s }
        Nothing -> Done (if p.ip == length instructions then Just p.acc else Nothing)

maybeFlipInstruction :: Array Instruction -> Int -> Maybe (Array Instruction)
maybeFlipInstruction instructions i = do
  ins <- index instructions i
  ins' <- case ins.op of
    Nop -> pure ins { op = Jmp }
    Jmp -> pure ins { op = Nop }
    _   -> Nothing
  updateAt i ins' instructions

allFlips :: Array Instruction -> Array (Array Instruction)
allFlips instructions = mapMaybe (maybeFlipInstruction instructions) (range 0 (length instructions - 1))

flipUntilSuccess :: Array Instruction -> Maybe Int
flipUntilSuccess = alaF First foldMap runOrFail <<< allFlips

step :: Processor -> Instruction -> Processor
step p = case _ of
  { op: Nop } -> p { ip = p.ip + 1 }
  { op: Jmp, arg: n } -> p { ip = p.ip + n }
  { op: Acc, arg: n } -> p { ip = p.ip + 1, acc = p.acc + n }

intParser :: Parser Int
intParser =
  regex """\d+""" >>= maybe (fail "Invalid int") pure <<< Int.fromString

instructionParser :: Parser Instruction
instructionParser = do
  op <- choice
    [ const Nop <$> string "nop"
    , const Acc <$> string "acc"
    , const Jmp <$> string "jmp"
    ]
  scale <- choice
    [ const identity <$> string " +"
    , const negate <$> string " -"
    ]
  arg <- scale <$> intParser
  pure { op: op, arg: arg }

inputParser :: Parser (Array Instruction)
inputParser = fromFoldable <$> sepEndBy instructionParser (regex """\r?\n""") <* eof

defaultInput :: String
defaultInput = """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"""

main :: Effect Unit
main = launchAff_ do
  inputText <- fromMaybe defaultInput <$> readStdin
  input <- either (liftEffect <<< throw <<< show) pure $ runParser inputParser inputText
  logShow <<< runUntilLoop $ input
  logShow <<< flipUntilSuccess $ input
