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

cloth = Cloth.rectCloth (-160, -60) (8, 1) 1 40 1


port log: Signal String
port log = Signal.map (\gs ->
        let pointmasses = gs.cloth.pointmasses
            velocities = map (\ptms -> ptms.velocity) pointmasses
        in toString <| List.map (\ v -> 
            ( String.left 3 (toString v.x), 
              String.left 3 (toString v.y) )) (toList velocities)
        ) 
        (Signal.sampleOn (every second) gameState)
