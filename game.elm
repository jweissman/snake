import Serpent
import Apple
import Config exposing (..)

import Geometry exposing (..)

import Html exposing (Html)
import Html.App as App

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Keyboard

import Time
import Random

-- MAIN
main =
  App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL
type alias Model =
  { player : Serpent.Model
  , keyCode : Int
  , apples : List Apple.Model
  , rivals : List Serpent.Model
  }

-- INIT
init : (Model, Cmd Msg)
init = let
         idToTossCommand =
           (\id -> (Random.generate (TossApple id) (Geometry.randomPosition fieldDim)))
         newApple =
           (\id -> (Apple.init id {x=5,y=5}))
         commands = (List.map idToTossCommand [1..appleCount])

         rivals = List.map (\n->Serpent.init [{x=(n*2)+30,y=(n*2)+20}] True n) [1..rivalCount]
       in
          (
            { player = (Serpent.init [{x=3,y=3}] False 0)
            , keyCode = -1
            , apples = List.map newApple [1..appleCount]
            , rivals = rivals
            },

            Cmd.batch commands
          )

-- UPDATE
type Msg
  = Tick Time.Time
  | KeyMsg Keyboard.KeyCode
  | TossApple Int Position
  --| TurnRival Int Orientation

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Tick _ ->
      let
        origin =
          { x = 0, y = 0 }

        headPosition =
          Maybe.withDefault origin (List.head model.player.segments)

        apples =
          List.map (Apple.update (Apple.Age)) model.apples

        rivals =
          List.map (Serpent.update (Serpent.Slither)) model.rivals

        isConsumed =
          \apple -> headPosition == apple.position

        isRotten =
          \apple -> not (Apple.isFresh apple)

        rottenApples =
          (List.filter isRotten apples)
      in
        if List.any isConsumed apples then
          -- ...'consume' apple! give it a new random position
          let
            matchingApple = Maybe.withDefault (Apple.init -1 origin) (List.head (List.filter isConsumed apples))
          in
            ({ model | player = Serpent.update (Serpent.Grow (Apple.growthFactor matchingApple)) model.player
             , apples = apples
             , rivals = rivals
             }, Random.generate (TossApple matchingApple.id) (Geometry.randomPosition fieldDim))

        else -- normal slither/update
          let
            tossAppleCommand =
              \apple ->
                Random.generate (TossApple apple.id) (Geometry.randomPosition fieldDim)

            -- turnRivalCommand =
            --   \rival ->
            --     Random.generate (TurnRival rival.id) (Geometry.randomOrientation)

            commandList = List.map tossAppleCommand rottenApples
          in
             ({ model | player = Serpent.update (Serpent.Slither) model.player
              , apples = apples
              , rivals = rivals
             }, Cmd.batch commandList)

    TossApple id position ->
      let
        apples = List.map (tossMatchingApple id position) model.apples
      in
        ({ model | apples = apples }, Cmd.none)

    KeyMsg code ->
      let
        orientation = case code of
          97 -> Left
          100 -> Right
          _ -> Straight
        updatedPlayer =
          Serpent.update (Serpent.Turn orientation) model.player
      in
        ({model | player = updatedPlayer }, Cmd.none)

tossMatchingApple id position apple =
  if apple.id == id then
    Apple.update (Apple.Toss position) apple
  else
    apple

-- SUBS
interval = 30 * Time.millisecond

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
     [ Time.every interval Tick
     , Keyboard.presses KeyMsg
     ]

-- VIEW
canvasSize = "800px"
view : Model -> Html Msg
view model =
  let
    background = rect [ x "0", y "0", width canvasSize, height canvasSize, fill "#a8b8c8" ] []
    appleViews = List.map Apple.view model.apples
    playerView = Serpent.view model.player
    rivalViews = List.concatMap (Serpent.view) model.rivals
    scoreMessage =
      (toString ((List.length (model.player.segments))))

    scoreHeader =
      text' [ x "2", y "6", fontFamily "Helvetica", fontSize "5", fill "rgba(240,240,240,0.5)" ] [ Html.text "LENGTH" ]
    scoreText =
      text' [ x "6", y "24", fontFamily "Helvetica", fontSize "18", fill "rgba(240,240,240,0.5)" ] [ Html.text scoreMessage ]
    scoreBoard =
      rect [ x "2", y "8", width "50px", height "20px", fill "rgba(240,160,160,0.5)" ] [ ]

    viewDims = "0 0 " ++ (toString fieldDim) ++ " " ++ (toString fieldDim)

  in
    svg [ viewBox viewDims, width canvasSize, height canvasSize ] ([background] ++ appleViews ++ playerView ++ rivalViews ++ [scoreBoard, scoreHeader, scoreText])
