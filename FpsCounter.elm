module FpsCounter (fpsCounter, createFpsCounter) where

import List(foldl, length, map2, (::), take)
import Signal(Signal, sampleOn, foldp)
import Time(fps)
import Graphics.Element(Element, beside)
import Graphics.Collage(collage, traced, solid, path)
import Color(black)
import Text(..)


{-| A frame rate counter chart for your Elm program.
@docs fpsCounter, createFpsCounter
-}

average : List Float -> Float
average xs = foldl (+) 0 xs |> (/) (toFloat (length xs))

sixty = [1..60]

{-| Measures fps of the program and creates a real time updating element -}
fpsCounter : Int -> Int -> Signal Element
fpsCounter f n = sampleOn 
                    (fps f)
                    createFpsCounter f 
                        (sampleOn (fps 3) remember n)

{-| Creates a fps counter
* First argument is frames per second
* Second argument is a list of the time between frames in milliseconds
-}
createFpsCounter : Int ->  List Float -> Element
createFpsCounter f xs = plainText (toString (average xs))

remember: Int -> Signal (List Float)
remember t = foldp (\x xs -> 1000 / x :: take (t-1) xs) []