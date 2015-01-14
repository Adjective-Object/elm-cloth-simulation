import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Time (inSeconds, fps, every, second)
import Signal
import Array(Array, map, indexedMap, foldl,
              length, get, fromList, empty, 
              toList, append)
import Cloth


InitialState  = rectCloth (-30, -45) (3, 4) 1 30 1

delta : Signal Float
delta = Time.fps 30

input : Signal Input
input = Signal.sampleOn delta (Signal.map2 Input delta userInput)

gameState : Signal gameState
gamestate = Signal.foldp step initialState input

step dt cloth =
 let newcloth = updateCloth maincloth dt
 in {maincloth | 
      pointmasses <- newcloth.pointmasses}

view : Cloth->Element
view maincloth = 
  collage 600 600
    [ rect 300 300
            |> filled (rgb 46 9 39)
    , drawCloth maincloth (rgb 217 0 0)
    ]

port log:Signal String
port log = Signal.map 
            (\t -> 
              "positions:" ++
              (foldl
                (++) ""
                  (map (\ptms -> 
                    toString (ptms.loc.x, 
                              ptms.loc.y)) 
                    maincloth.pointmasses)) 
              ++ "\nvelocities: " ++
              (foldl 
                (++) ""
                  (map (\ptms -> 
                    toString (ptms.velocity.x, 
                              ptms.velocity.y)) 
                    maincloth.pointmasses))
              )
              (every (second))

{-
port log: Signal String
port log = Signal.map (\t ->
        let updatedCloth = step 0.1 maincloth
            positions = map (\ptms -> ptms.loc) updatedCloth.pointmasses
        in toString (toList positions)
      ) (every second)
-}



main : Signal Element
main = Signal.map inSeconds (fps 30)
        |> Signal.foldp step maincloth
        |> Signal.map view

