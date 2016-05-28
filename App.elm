import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import List
import Window
import Text
import Task
import Html exposing (Html)
import Html.App exposing (program)
import AnimationFrame exposing (..)
import Time exposing (Time)
import String
import Array exposing (Array, map, indexedMap, foldl,
              length, get, fromList, empty,
              toList, append)
import Cloth
import Platform.Cmd
import Primitives exposing (Point)

-- define the gamestate
type alias GameState = 
  { cloth: Cloth.Cloth
  , frame: {width: Int, height: Int}
  }

initialState : GameState
initialState = 
  { cloth = Cloth.rectCloth 
      { offsets = (0, 0)
      , dimensions = (13, 6)
      , point_mass = 0.1
      , spring_len = 20
      , spring_k = 3
      , damping_factor = 0.985
      , gravity = Point 0 -19.6
      , fixed_points = [(0, 0), (12, 0)]
      }
  , frame = {width = 800, height = 600}
  }

type Input 
  = Tick Time
  | Resize Window.Size

-- step model
step: Input -> GameState -> GameState
step msg state = 
  case msg of 
  Tick dt     -> {state | cloth = Cloth.updateCloth state.cloth dt }
  Resize size -> {state | frame = size }

-- render gamestate at width and height
render : GameState -> Form
render state = Cloth.drawCloth state.cloth (rgb 217 0 0)

view : GameState -> Html Input
view model = collage
  model.frame.width model.frame.height
  [ rect 
    (toFloat model.frame.width) 
    (toFloat model.frame.height) |> filled black
  , render model
  , text (Text.fromString "THIS IS A GAME"
            |> Text.color red)
  ]
  |> Element.toHtml 

subscriptions : GameState -> Sub Input
subscriptions state = Sub.batch 
  [ diffs Tick
  , Window.resizes Resize 
  ]


fireInit = Task.perform 
  (\ size  -> Resize size)
  (\ size  -> Resize size)
  Window.size

main = program
    { init = (initialState, fireInit)
    , update = \msg model -> (step msg model, Cmd.none)
    , subscriptions = subscriptions
    , view = view
    }
