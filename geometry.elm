module Geometry exposing (Orientation(..), Direction(..), orientDirection, randomOrientation, Position, coordinate, randomPosition)

import Random

type Orientation = Left | Right | Straight
type Direction = North | East | South | West

type alias Position =
  { x : Int
  , y : Int
  }

orientDirection : Orientation -> Direction -> Direction
orientDirection orient dir =
  let
    isLeft = orient == Left
  in
    if orient == Straight then
      dir
    else
      case dir of
        South -> if isLeft then East else West
        North -> if isLeft then West else East
        East  -> if isLeft then North else South
        West  -> if isLeft then South else North

randomOrientation : Random.Generator Orientation
randomOrientation =
  Random.map (\t ->
    if t == 0 then
      Left
    else
      if t == 1 then
        Right
      else
        Straight
    ) (Random.int 0 2)

coordinate : Position -> Direction -> Position
coordinate {x,y} dir =
  case dir of
    North -> {x=x, y=y-1}
    East  -> {x=x+1, y=y}
    South -> {x=x, y=y+1}
    West  -> {x=x-1, y=y}

randomPosition : Int -> Random.Generator Position
randomPosition range =
  let
    randomIntWithinRange = (Random.int 0 range)
  in
    Random.map2 (\x -> (\y -> { x = x, y = y })) (randomIntWithinRange) (randomIntWithinRange)
    --{ x = randomIntWithinRange, y = randomIntWithinRange }

