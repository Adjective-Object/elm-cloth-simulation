module Utils exposing (..)  -- comment for synax highlighting where


{-|
Utilities module
-}

import Array exposing (Array, get, indexedMap, fromList, toList)
import List
import Color exposing (Color, hsla)

import Native.OptMath

zip : Array a -> Array b -> Array (a,b)
zip a b = fromList <| List.map2 (,) (toList a) (toList b)

zip3 : Array a -> Array b -> Array c -> Array (a,b,c)
zip3 a b c = fromList 
    <| List.map3 (\a b c -> (a, b, c)) (toList a) (toList b) (toList c)

sqrt = Native.OptMath.sqrt 

{-| Array of size l, filled with a specified value -}
filledArray:x->Int->Array x
filledArray value l = indexedMap (\ index n -> value) (fromList [0..l]) 
