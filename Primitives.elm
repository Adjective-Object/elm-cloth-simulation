module Primitives where

import Array(Array, map, indexedMap, foldl,
              length, get, fromList, empty, 
              toList, append)

type alias Point = {x:Float, y:Float}
type alias PointMass = {
  loc:Point, velocity:Point, mass:Float, fixed:Bool, last_force: Point}


default_pointmass = { loc = Point 0 0
                    , velocity = Point 0 0
                    , mass = 1
                    , fixed = True
                    , last_force = Point 0 0}

origin = Point 0 0


addPoints:Point->Point->Point
addPoints a b = Point ((a.x)+(b.x)) ((a.y)+(b.y))

diffPoints:Point->Point->Point
diffPoints a b = Point ((a.x)-(b.x)) ((a.y)-(b.y))

invertPoint:Point->Point
invertPoint p = Point -p.x -p.y

scalepoint:Float->Point->Point
scalepoint scale p = Point (scale*p.x) (scale*p.y)

similarTri:Point->Float->Point
similarTri original hypotenuse = 
    let original_hypotenuse = dist origin original
        ratio = hypotenuse / original_hypotenuse
    in scalepoint ratio original

filledArray:Point->Int->Array Point
filledArray value l= indexedMap (\ index n -> value) (fromList [0..l]) 

gravityArray:Array PointMass->Array Point
gravityArray ptmasses = 
  map (\ ptmas -> scalepoint ptmas.mass (Point 0 -9.8)) ptmasses 


-- gets the distance between two points
-- power maintains negatives, so abs
dist:Point->Point->Float
dist a b = sqrt((a.x-b.x)^2 + (b.y-a.y)^2)

-- gets the angle between the two points
angle:Point->Point->Float
angle a b = atan2 (b.y - a.y) (b.x - a.x)

-- gets rotates point p by angle a about the origin
rotatepoint:Point->Float->Point
rotatepoint p ang = let theta = -pi / 2 - ang
                        xs = p.x * sin theta
                        xc = p.x * cos theta
                        ys = p.y * sin theta
                        yc = p.y * cos theta
                  in (Point (xc - ys) (xs + yc))