module Primitives exposing (..)

{-|
provides primitive math & physics objets, as well as methods for manipulating
them

@docs Point
@docs PointMass
@docs default_pointmass
@docs origin
@docs addPoints
@docs diffPoints
@docs invertPoint
@docs scalePoint
@docs similarTri
@docs gravityArray
@docs dist
@docs angle
@docs rotatepoint
-}

-- comment for synax highlighting where

import Array
    exposing
        ( Array
        , map
        , indexedMap
        , foldl
        , length
        , get
        , fromList
        , empty
        , toList
        , append
        )
import Utils exposing (fastSqrt)


{-| 2 dimensional point
-}
type alias Point =
    { x : Float, y : Float }


{-| Basic physics primitive.
Can be fixed (unmoving).
Keeps track of force applied to it on last frame
-}
type alias PointMass =
    { loc : Point
    , velocity : Point
    , mass : Float
    , fixed : Bool
    , last_force : Point
    }


{-| fixed pointmass @ origin with no velocity or force, and a mass of 1
-}
default_pointmass : PointMass
default_pointmass =
    { loc = Point 0 0
    , velocity = Point 0 0
    , mass = 1
    , fixed = True
    , last_force = Point 0 0
    }


{-| Point @ (0,0)
-}
origin : Point
origin =
    Point 0 0


{-| Adds two points together
-}
addPoints : Point -> Point -> Point
addPoints a b =
    Point ((a.x) + (b.x)) ((a.y) + (b.y))


{-| Takes the difference between two points
-}
diffPoints : Point -> Point -> Point
diffPoints a b =
    Point ((a.x) - (b.x)) ((a.y) - (b.y))


{-| inverts a point about the origin
-}
invertPoint : Point -> Point
invertPoint p =
    Point -p.x -p.y


{-| scales a point away from the origin by some ratio
-}
scalePoint : Float -> Point -> Point
scalePoint scale p =
    Point (scale * p.x) (scale * p.y)


{-| finds a similar right triangle to the specified one,
but with the supplied hypotenuse
-}
similarTri : Point -> Float -> Point
similarTri original hypotenuse =
    let
        original_hypotenuse =
            dist origin original

        ratio =
            hypotenuse / original_hypotenuse
    in
        scalePoint ratio original


{-| finds the force of gravity applied by each point to each pointmass
-}
gravityArray : Array PointMass -> Float -> Array Point
gravityArray ptmasses gravity =
    map (\ptmas -> scalePoint ptmas.mass (Point 0 gravity)) ptmasses


{-| gets the distance between two points
-}
dist : Point -> Point -> Float
dist a b =
    fastSqrt ((a.x - b.x) ^ 2 + (b.y - a.y) ^ 2)


{-| gets the angle between the two points
-}
angle : Point -> Point -> Float
angle a b =
    atan2 (b.y - a.y) (b.x - a.x)


{-| gets rotates point p by angle a about the origin
-}
rotatepoint : Point -> Float -> Point
rotatepoint p ang =
    let
        theta =
            -pi / 2 - ang

        xs =
            p.x * sin theta

        xc =
            p.x * cos theta

        ys =
            p.y * sin theta

        yc =
            p.y * cos theta
    in
        (Point (xc - ys) (xs + yc))
