import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import List
import Html exposing (Html)
import Html.App exposing (program)
import AnimationFrame exposing (..)
import Time exposing (Time)
import String
import Array exposing (Array, map, indexedMap, foldl,
              length, get, fromList, empty,
              toList, append)
import Cloth

-- define the gamestate
type alias GameState = {cloth: Cloth.Cloth}

initialState : GameState
initialState  = { cloth = Cloth.rectCloth (-240, 0) (6, 4) 0.5 40 50 0.995}

type Input = Tick Time

-- step model
step: Input -> GameState -> GameState
step (Tick dt) state =
  {state | cloth = Cloth.updateCloth state.cloth }

-- render gamestate at width and height
render : (Int,Int) -> GameState -> Element
render (w,h) state =
  collage w h
    [ rect (toFloat w) (toFloat h)
            |> filled (rgb 46 9 39)
    , Cloth.drawCloth state.cloth (rgb 217 0 0)
    , (show "THIS IS A GAME"
              --|> Text.color (rgb 217 0 0)
              --|> Text.height 36
      )
    ]

view : GameState -> Html Input
view model = collage
  (floor width) (floor height)
  [ rect width height |> filled black
  , render (width, height) model
  ]
  |> Element.toHtml 

subscriptions : GameState -> Sub Input
subscriptions state = diffs Tick


main = program
    { init = (initialState, Cmd.none)
    , update = \msg model -> (step msg model, Cmd.none)
    , subscriptions = subscriptions
    , view = render
    }
