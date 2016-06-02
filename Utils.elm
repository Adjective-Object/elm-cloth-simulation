module Utils exposing (..)

{-|
Utilities module
-}

-- where

import Array exposing (Array, get, indexedMap, fromList, toList)
import List
import Color exposing (Color, hsla)
import Native.OptMath exposing (sqrt)


zip : Array a -> Array b -> Array ( a, b )
zip a b =
    fromList <| List.map2 (,) (toList a) (toList b)


zip3 : Array a -> Array b -> Array c -> Array ( a, b, c )
zip3 a b c =
    fromList
        <| List.map3 (\a b c -> ( a, b, c )) (toList a) (toList b) (toList c)


fastSqrt = sqrt


{-| Array of size l, filled with a specified value
-}
filledArray : x -> Int -> Array x
filledArray value l =
    indexedMap (\index n -> value) (fromList [0..l])


indexList : List a -> List ( Int, a )
indexList a =
    List.indexedMap (,) a


indexArray : Array a -> Array ( Int, a )
indexArray a =
    Array.indexedMap (,) a


(!|) : Array a -> Int -> a
(!|) arr ind =
    case get ind arr of
        Just x ->
            x

        Nothing ->
            Debug.crash
                <| "crash because we tried to get ["
                ++ toString ind
                ++ "] from array len "
                ++ toString (Array.length arr)
                ++ "\n\n"
                ++ toString arr
