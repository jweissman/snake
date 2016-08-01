module Serpent exposing (Model, Msg(..), init, update, view, createSerpent)

import Config exposing (..)

import Geometry exposing (..)
import Html exposing (Html)

import Random

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Time

-- MODEL

type alias Model =
  { direction : Direction
  , segments : List Position
  , growthRemaining : Int
  , isRival : Bool
  , id : Int
  }

-- INIT

init : List Position -> Bool -> Int -> Model
init segments isRival id =
  createSerpent South segments id isRival

createSerpent : Direction -> List Position -> Int -> Bool -> Model
createSerpent direction segments id isRival =
  { direction = direction
  , segments = segments
  , growthRemaining = 9
  , isRival = isRival
  , id = id
  }

-- UPDATE
type Msg
  = Slither
  | Turn Orientation
  | Grow Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Slither ->
      slither model

    Grow factor ->
      grow model factor

    Turn orientation ->
      let
        newDirection = orientDirection orientation model.direction
      in
        {model | direction = newDirection}

-- update helpers
wrapPosition : Int -> Position -> Position
wrapPosition sz {x,y} =
  { x = x % sz, y = y % sz }

grow : Model -> Int -> Model
grow snake factor =
  let
    origin = {x=0,y=0}
    oldHead = Maybe.withDefault origin (List.head snake.segments)
    newHead = wrapPosition (fieldDim//cellSize) (coordinate oldHead snake.direction)
    newBody = Maybe.withDefault [] (List.tail snake.segments)
  in
    { snake | segments = newHead :: oldHead :: newBody, growthRemaining = factor - 1  }


slither : Model -> Model
slither snake =
  if snake.growthRemaining > 0 then
    -- grow instead of slither
    grow snake snake.growthRemaining
  else
    let
      origin = {x=2,y=2}
      oldHead = Maybe.withDefault origin (List.head snake.segments)
      newHead = wrapPosition (fieldDim//cellSize) (coordinate oldHead snake.direction)
      newBody = (innerList snake.segments)
    in
      { snake | segments = newHead :: oldHead :: newBody }


innerList : List a -> List a
innerList list =
  let
    tail = Maybe.withDefault [] ( List.tail(list))
    reversedTail = List.reverse(tail)
  in
    List.reverse(
      Maybe.withDefault [] (
        List.tail(reversedTail)
      )
    )


-- VIEW

view : Model -> List (Svg a)
view model =
  let
    snakeParts = List.reverse model.segments
  in
    List.indexedMap (segmentView model) snakeParts

segmentView : Model -> Int -> Position -> Svg a
segmentView model idx pos =
  let
    len = List.length model.segments
    x' = (toString (pos.x*cellSize))
    y' = (toString (pos.y*cellSize))

    distance =
      round (((toFloat idx) / (toFloat len)) * 9)

    baseRed =
      if model.isRival then "a3" else "18"

    baseBlue =
      if model.isRival then "b5" else "20"

    color =
      "#" ++ baseRed ++ (toString (distance)) ++ "f" ++ baseBlue
  in
    rect [ x x'
       , y y'
       , width (toString (cellSize))
       , height (toString (cellSize))
       , fill color
       ] []
