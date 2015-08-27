module Utils where

{-|
Utilities module
-}

import Array exposing (Array, get, toList, indexedMap)
import List
import Color exposing (..)

import Native.OptMath

-- cuz im lazy and dont want to unpack maybes in contexts I know are safe
(!|):(Array a)->Int->a
(!|) arr ind = case (get ind arr) of
                  Just x -> x
                  _ -> (List.head (toList arr))

zip : Array a -> Array b -> Array (a,b)
zip aX aY = indexedMap (\ i x -> (x, (aY !| i)) ) aX

zip3 : Array a -> Array b -> Array c-> Array (a,b,c)
zip3 aX aY aZ= indexedMap (\ i x -> (x, (aY !| i), (aZ !| i)) ) aX

inversecolor : Color -> Color
inversecolor c = case c of
                  RGBA r g b a -> RGBA (255-r) (255-g) (255-b) a
                  HSLA h s l a -> let (rf, gf, bf) = (hslToRgb h s l)
                                      r = round (255 * rf)
                                      g = round (255 * gf)
                                      b = round (255 * bf)
                                  in inversecolor (RGBA r g b a)

sqrt = Native.OptMath.sqrt
