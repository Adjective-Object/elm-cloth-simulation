module Utils where

{-|
Utilities module
-}

import Array exposing (Array, get, toList, indexedMap)
import List
import Color exposing (Color, hsla)

import Native.OptMath

-- cuz im lazy and dont want to unpack maybes in contexts I know are safe
(!|):(Array a)->Int->a
(!|) arr ind = case (get ind arr) of
                  Just x -> x

zip : Array a -> Array b -> Array (a,b)
zip aX aY = indexedMap (\ i x -> (x, (aY !| i)) ) aX

zip3 : Array a -> Array b -> Array c-> Array (a,b,c)
zip3 aX aY aZ= indexedMap (\ i x -> (x, (aY !| i), (aZ !| i)) ) aX

sqrt = Native.OptMath.sqrt
