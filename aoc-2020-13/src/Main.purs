module Main where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.MonadZero (guard)
import Data.Array (fold, foldMap, index)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
import Input (readStdin)

type BusNotes = { currentTime :: BigInt, ids :: Array (Maybe BigInt) }

parseBusNotes :: Array String -> Maybe BusNotes
parseBusNotes lines = do
  l0 <- index lines 0
  l1 <- index lines 1
  currentTime <- BigInt.fromString l0
  pure { currentTime, ids: map BigInt.fromString $ split (Pattern ",") l1 }

data BusTime = BusTime { id :: BigInt, delay :: BigInt }
instance semigroupBusTime :: Semigroup BusTime where 
  append a@(BusTime { delay: ad }) b@(BusTime { delay: bd }) = if ad <= bd then a else b
instance showBusTime :: Show BusTime where
  show (BusTime { id, delay }) = fold
    [ "BusTime { id: "
    , BigInt.toString id
    , ", delay: "
    , BigInt.toString delay
    , ", mul: " <> BigInt.toString (id * delay)
    , " }"
    ]

minBusTime :: BusNotes -> Maybe BusTime
minBusTime notes = foldMap go notes.ids
  where
    go maybeTime = do
      id <- maybeTime
      let minMod = mod notes.currentTime id
      let delay = if minMod == zero then zero else id - minMod
      Just $ BusTime { id, delay }

-- bruteForceTime :: BusNotes -> Maybe BigInt
-- bruteForceTime notes = map (\x -> tailRec (go x) zero) (Map.findMax idMap)
--   where
--     go { key, value } n =
--       let t = n * key + value
--       in if allWithIndex (runsAt t) idMap then Done t else Loop (n + one)
--     runsAt t k v = mod t k == v
--     idMap =
--       let insertId i acc = maybe acc (\ k -> Map.insert k (mod (k - mod (BigInt.fromInt i) k) k) acc)
--       in foldlWithIndex insertId Map.empty notes.ids

phaseDifferenceTime :: BusNotes -> Maybe (PeriodPhase BigInt)
phaseDifferenceTime notes = foldWithIndexM go { period: one, phase: zero } notes.ids
  where
    go :: Int -> PeriodPhase BigInt -> Maybe BigInt -> Maybe (PeriodPhase BigInt)
    go i acc = maybe (Just acc) \period -> combinedPhase acc { period, phase: mod (negate (BigInt.fromInt i)) period }

extendedGcd :: forall a. EuclideanRing a => Eq a => a -> a -> { gcd :: a, s :: a, t :: a }
extendedGcd a b = tailRec go { oldR: a, r: b, oldS: one, s: zero, oldT: zero, t: one }
  where
    go step@{ r, oldR, s, oldS, t, oldT } =
      if r == zero
      then Done { gcd: oldR, s: oldS, t: oldT }
      else let  quotient = div oldR r
                remainder = mod oldR r
      in Loop { oldR: r
              , oldS: s
              , oldT: t
              , r: remainder
              , s: oldS - quotient * s
              , t: oldT - quotient * t
              }

type PeriodPhase a = { period :: a, phase :: a }

combinedPhase :: forall a. EuclideanRing a => Eq a => PeriodPhase a -> PeriodPhase a -> Maybe (PeriodPhase a)
combinedPhase a b = do
  let e = extendedGcd a.period b.period
  let phaseDifference = a.phase - b.phase
  guard (mod phaseDifference e.gcd == zero)
  let pdMult = div phaseDifference e.gcd
  let period = div a.period e.gcd * b.period
  Just { period, phase: mod (a.phase - e.s * pdMult * a.period) period }

defaultInput :: String
defaultInput = """939
7,13,x,x,59,x,31,19"""

main :: Effect Unit
main = launchAff_ do
  inputText <- fromMaybe defaultInput <$> readStdin
  let input = parseBusNotes $ split (Pattern "\n") inputText
  logShow $ input >>= minBusTime
  logShow do
    p <- input
    k <- phaseDifferenceTime p
    pure k.phase
