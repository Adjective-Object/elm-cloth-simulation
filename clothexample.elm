import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Graphics.Input(..)
import List
import Time (inSeconds, fps, every, second)
import Signal
import String
import Mouse
import Window
import Array(Array, map, indexedMap, foldl,
              length, get, fromList, empty,
              toList, append)
import Cloth


-- modelling the input types
type alias UserInput =
    {mouseCoords: (Int, Int), mouseDown: Bool, mouseClicked: Bool}
defaultUserInput =
    {mouseCoords = (-1,-1), mouseDown = False, mouseClicked = False}

type UserInputSignifier = Coordinate | MouseClick | MouseDown
type alias InputUpdate =
    {updatedField:UserInputSignifier, values:UserInput}

advanceUserInput: InputUpdate -> UserInput -> UserInput
advanceUserInput update oldInput =
    case update.updatedField of
      _ -> oldInput
      Coordinate -> {oldInput | mouseCoords <- update.values.mouseCoords}
      MouseDown  -> {oldInput | mouseDown <- update.values.mouseDown}
      MouseClick -> {oldInput | 
                      mouseClicked <- update.values.mouseClicked
                      -- update mouseDown to deal with input events discarded by
                        -- mergeMany
                    , mouseDown <- if update.values.mouseClicked
                                    then True
                                    else oldInput.mouseDown}
      
userInput : Signal UserInput
userInput = Signal.foldp 
              advanceUserInput
              defaultUserInput
              -- unify the update signals under InputUpdate so
              -- they are all typed to default values
              <| Signal.mergeMany 
                [ -- track mouse position
                  Signal.map (\ pos ->
                      InputUpdate
                        Coordinate
                        {defaultUserInput | mouseCoords <- pos}) Mouse.position

                  -- track mouse click positions
                , Signal.map (\ click ->
                      InputUpdate
                        MouseClick
                        {defaultUserInput | mouseClicked <- True}) Mouse.clicks

                  -- track mouse button position
                , Signal.map (\ down ->
                      InputUpdate
                        MouseDown
                        {defaultUserInput | mouseDown <- down}) Mouse.isDown

                  -- clear the MouseDown handler 
                , Signal.map (\ down ->
                      InputUpdate
                        MouseClick
                        {defaultUserInput |mouseClicked <- False}) (fps 30)
                ]


type alias Input =
    { dt : Float
    , input : UserInput }




-- define the gamestate
type alias GameState = {cloth: Cloth.Cloth}

initialState : GameState
initialState  = { cloth = Cloth.rectCloth (-160, 0) (8, 5) 1 40 10}


step: Input -> GameState -> GameState
step {dt, input} state =
    let newcloth = Cloth.updateCloth state.cloth dt
    in {state | cloth <- newcloth}

render : (Int,Int) -> GameState -> Element
render (w,h) state =
  collage w h
    [ rect (toFloat w) (toFloat h)
            |> filled (rgb 46 9 39)
    , Cloth.drawCloth state.cloth (rgb 217 0 0)
    ]


-- setup the sampling framerate and use it to create an input signal
delta : Signal Float
delta = fps 60

input : Signal Input
input = Signal.sampleOn delta (Signal.map2 Input delta userInput)

-- generate discrete gamestate as signal sampling on input
gameState : Signal GameState
gameState = Signal.foldp step initialState input

-- map the render over the gameState signal
main : Signal Element
main = Signal.map2 render Window.dimensions gameState