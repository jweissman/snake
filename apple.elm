module Apple exposing (Model, Msg(..), init, update, view, isFresh, growthFactor)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Geometry
import Config exposing (cellSize)

-- MODEL

type Freshness = Great | Good | Okay | Bad

type alias Model =
  { position : Geometry.Position
  , freshness : Freshness
  , age : Int
  , id : Int
  }

-- INIT

init : Int -> Geometry.Position -> Model
init id position =
  { position  = position
  , freshness = Great
  , age = 0
  , id = id
  }

-- UPDATE

type Msg
  = Age
  | Toss Geometry.Position

update : Msg -> Model -> Model
update msg model =
  case msg of
    Toss position ->
      let
        x' = position.x // cellSize
        y' = position.y // cellSize
      in
        { model | position = {x=x', y=y'}
        , freshness = Great
        , age = 0
        }

    Age ->
      let
        age =
          model.age + 1

        freshness =
          freshnessByAge age
      in
         { model | age = age, freshness = freshness }


freshnessByAge : Int -> Freshness
freshnessByAge age =
  if age < 250 then
    Great
  else
    if age < 500 then
      Good
    else
     if age < 1000 then
       Okay
     else
      Bad



-- VIEW

appleRed = "#c09090"
darkRed = "#a06060"
lightBrown = "#803030"
darkGreen = "#309030"

view : Model -> Svg a
view model =
  let
    color' = case model.freshness of
      Great -> appleRed
      Good -> darkRed
      Okay -> lightBrown
      Bad -> darkGreen
      --_ -> lightBrown
  in
    rect [ x (toString (model.position.x * cellSize))
         , y (toString (model.position.y * cellSize))
         , width (toString (cellSize))
         , height (toString (cellSize))
         , fill color' ] []


-- helper

isFresh : Model -> Bool
isFresh apple =
  not (apple.freshness == Bad)

growthFactor : Model -> Int
growthFactor apple =
  case apple.freshness of
    Great -> 10
    Good -> 5
    Okay -> 1
    Bad -> -1
