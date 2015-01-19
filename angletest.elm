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
import Cloth(Point, angle, rotatepoint, scalepoint, dist)
import Text (asText)


main: Signal Element
main = Signal.map (\ (x,y) -> 
  let origin  = (Point 0 0)
      mousept = (Point (toFloat x-200) (toFloat y-200))
      ang     = (angle origin mousept)
      rpt     = (rotatepoint (Point 0 0.8) ang)
      dst     = dist (Point 0 0) mousept
      scl     = (scalepoint dst rpt)
  in collage 400 400
    [ move (0, -15) (toForm <| asText(ang))
    , traced (solid (rgb 46 9 39)) (path [(0,0), (scl.x, scl.y)])
    , filled black (circle 2)]) Mouse.position
