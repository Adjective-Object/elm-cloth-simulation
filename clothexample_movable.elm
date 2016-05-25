import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Graphics.Input(..)
import List(head, tail, map2)
import Time (inSeconds, fps, every, second)
import Signal
import String
import Mouse
import Window
import Array(Array, map, indexedMap, foldl,
              length, get, fromList, empty,
              toList, append, slice)
import Cloth
import Primitives(Point,dist)
import Utils(zip)

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
type alias GameState = {
    cloth: Cloth.Cloth, 
    grabbedPoint: Maybe Int}

initialState : GameState
initialState = { 
    cloth = Cloth.rectCloth (-160, 0) (8, 5) 30 40 500 0.995
  , grabbedPoint = Nothing}



-- helpers for grabbing the cloth

mouseToWorldPt : (Int,Int) -> Point
mouseToWorldPt (x,y) = 
  let (window_w, window_h) = Window.dimensions
  in (x - window_w/2) (window_h/2 - y)

nearestPoint : GameState -> Point -> Point
nearestPoint state mouse = 
  let pts = map (\ptms -> ptms.loc) state.cloth.pointmasses
      first_point = get 0 pts
      rest_points = slice 1 -1 pts
      shortest_pair : (Float, Point)
      shortest_pair = foldl 
                      (\ (old_dist, old_pt) new_pt -> 
                        let new_dist = dist new_pt mouse
                            smaller = (new_dist < old_dist)
                        in if smaller
                            then (new_dist, new_pt)
                            else (old_dist, old_pt))
                      (dist first_point mouse, first_point)
                      rest_points
  in  let (dist, val) = shortest_pair
      in val

freePoint : Int -> GameState -> GameState 
freePoint n state = 
  let cloth = state.cloth
      freedpts = 
        indexedMap 
          (\ ind val -> 
            if ind == n 
              then { val | fixed <- False }
              else val)
          state.cloth.pointmasses
      upcloth = { cloth | pointmasses <- freedpts}
  in { state | cloth <- upcloth }

grabCloth : UserInput -> GameState -> GameState
grabCloth input state =
  let statecloth = state.cloth
      mouse = mouseToWorldPt
      gPoint = 
        if input.mouseClicked
          then nearestPoint state mouse
          else if input.mouseDown
            then state.grabbedPoint
            else Nothing
  in case gPoint of
    Nothing -> case state.grabbedPoint of
                  Nothing -> statecloth
                  Just pt -> freePoint pt state 
    Just pt_ind -> 
        let newLocs : Array Point
            newLocs = indexedMap 
                        (\ index oldpt -> 
                            if index == pt_ind 
                              then mouse
                              else oldpt.loc)
                        (state.cloth.pointmasses)
            newPoints = map 
                        (\ (pt, loc) -> {pt | loc <- loc}) 
                        (zip state.cloth.pointmasses newLocs)
            cl = { statecloth | pointmasses <- newPoints}
        in {state | 
              cloth <- cl
            ,  grabbedPoint <- gPoint}


step: Input -> GameState -> GameState
step {dt, input} state =
    let 
      gstate = grabCloth input state
      newcloth = Cloth.updateCloth (gstate.cloth) dt
    in {gstate | cloth <- newcloth}

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
