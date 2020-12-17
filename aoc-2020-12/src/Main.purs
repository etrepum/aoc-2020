module Main where

import Prelude

import Data.Array (foldl, mapMaybe)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded as GBounded
import Data.Generic.Rep.Enum as GEnum
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.String (Pattern(..), drop, split)
import Data.String.CodeUnits (charAt)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
import Input (readStdin)

type WithCoordinate r = { x :: Int, y :: Int | r }

type Ferry = WithCoordinate ( heading :: Direction )

initialFerry :: Ferry
initialFerry = { heading: bottom, x: 0, y: 0 }

type Ship = WithCoordinate ( waypoint :: WithCoordinate () )

initialShip :: Ship
initialShip = { x: 0, y: 0, waypoint: { x: 10, y: -1 } }

data Direction = East | South | West | North
derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction
derive instance genericDirection :: Generic Direction _
instance showDirection :: Show Direction where
  show = genericShow
instance boundedDirection :: Bounded Direction where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumDirection :: Enum Direction where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumDirection :: BoundedEnum Direction where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum

data Command
  = Go Direction Int
  | Turn Int
  | Forward Int
derive instance eqCommand :: Eq Command
derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where
  show = genericShow

turn :: Direction -> Int -> Direction
turn d n = fromMaybe East <<< toEnum $ mod (n + fromEnum d) 4

stepCommand :: Ferry -> Command -> Ferry
stepCommand f = case _ of
  Go d n -> addCoordinate f $ makeCoordinate d n
  Turn n -> f{ heading = turn f.heading n }
  Forward n -> addCoordinate f $ makeCoordinate f.heading n

stepShipCommand :: Ship -> Command -> Ship
stepShipCommand f = case _ of
  Go d n -> f{ waypoint = addCoordinate f.waypoint (makeCoordinate d n) }
  Turn n -> f{ waypoint = rotateCoordinate f.waypoint n }
  Forward n -> addCoordinate f (scaleCoordinate f.waypoint n)

addCoordinate :: forall r. WithCoordinate r -> WithCoordinate () -> WithCoordinate r
addCoordinate s v = s{ x = s.x + v.x, y = s.y + v.y }

scaleCoordinate :: WithCoordinate () -> Int -> WithCoordinate ()
scaleCoordinate c n = { x: n * c.x, y: n * c.y }

rotateCoordinate :: WithCoordinate () -> Int -> WithCoordinate ()
rotateCoordinate c n = case mod n 4 of
  1 -> { x: negate c.y, y: c.x}
  2 -> { x: negate c.x, y: negate c.y }
  3 -> { x: c.y, y: negate c.x }
  _ -> c

makeCoordinate :: Direction -> Int -> WithCoordinate ()
makeCoordinate d n = case d of
  East -> { x: n, y: 0 }
  West -> { x: negate n, y: 0 }
  North -> { x: 0, y: negate n }
  South -> { x: 0, y: n }

degreesTo90Turns :: Int -> Maybe Int
degreesTo90Turns n = if mod n 90 /= 0 then Nothing else Just (div n 90)

readCommand :: String -> Maybe Command
readCommand s = do
  c <- charAt 0 s
  v <- Int.fromString $ drop 1 s
  case c of
    'N' -> pure $ Go North v
    'S' -> pure $ Go South v
    'E' -> pure $ Go East v
    'W' -> pure $ Go West v
    'F' -> pure $ Forward v
    'L' -> Turn <<< negate <$> degreesTo90Turns v
    'R' -> Turn <$> degreesTo90Turns v
    _ -> Nothing

defaultInput :: String
defaultInput = """F10
N3
F7
R90
F11"""

manhattanDistance :: forall r. WithCoordinate r -> Int
manhattanDistance c = abs c.x + abs c.y

main :: Effect Unit
main = launchAff_ do
  inputText <- fromMaybe defaultInput <$> readStdin
  let input = mapMaybe readCommand $ split (Pattern "\n") inputText
  logShow <<< manhattanDistance $ foldl stepCommand initialFerry input
  logShow <<< manhattanDistance $ foldl stepShipCommand initialShip input
