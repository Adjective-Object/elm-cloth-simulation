module Cloth where

import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Time (inSeconds, fps, every, second)
import Signal
import Array(Array, map, indexedMap, foldl,
              length, get, fromList, empty, 
              toList, append)

-- cuz im lazy and dont want to unpack maybes in contexts I know are safe
(!|):(Array a)->Int->a
(!|) arr ind = case (get ind arr) of
                  Just x -> x
                  _ -> (List.head (toList arr))

type alias Point = {x:Float, y:Float}
type alias PointMass = {loc:Point, velocity:Point, mass:Float, fixed:Bool}
type alias Spring = {index_a:Int, index_b:Int, k:Float, len:Float}
type alias Cloth = {pointmasses:Array PointMass, springs:Array Spring}

default_pointmass = { loc = Point 0 0,
                      velocity = Point 0 0,
                      mass = 1,
                      fixed = True}

default_spring = {  index_a = -1,
                    index_b = -1,
                    k = 1.0,
                    len = 1.0}


addPoints:Point->Point->Point
addPoints a b = Point ((a.x)+(b.x)) ((a.y)+(b.y))

invertPoint:Point->Point
invertPoint p = Point -p.x -p.y

scalePoint:Float->Point->Point
scalePoint scale p = Point (scale*p.x) (scale*p.y)

filledArray:Point->Int->Array Point
filledArray value l= indexedMap (\ index n -> value) (fromList [0..l]) 

gravityArray:Int->Array Point
gravityArray = filledArray (Point 0 -9.8)

geta:Array PointMass->Spring->PointMass
geta points spring = points !| (spring.index_a)

getb:Array PointMass->Spring->PointMass
getb points spring = points !| (spring.index_b)

-- gets the distance between two points
dist:Point->Point->Float
dist a b = sqrt((abs a.x-b.x)^2 + (abs a.y-b.y)^2)

-- gets the angle between the two points
angle:Point->Point->Float
angle a b = atan2 (a.x-b.x) (a.y-b.y)

-- gets rotates point p by angle a about the origin
rotatepoint:Point->Float->Point
rotatepoint p a = let xs = sin p.x
                      xc = cos p.x
                      ys = sin p.y
                      yc = cos p.y
                  in (Point (xc - ys) (xs - yc))

-- returns the force the spring exterts on pointmass a
-- the force exerted on pointmass b is just the inverse of a.
springForce:Spring->Array PointMass->Point
springForce s pointdb = let aloc = (geta pointdb s).loc
                            bloc = (getb pointdb s).loc
                            realdistance = dist aloc bloc
                            rawforce = s.k * (realdistance - s.len)
                        in rotatepoint 
                             (Point rawforce 0)
                             (angle aloc bloc)

-- applies a force to the two points given by i
applyForce:Point->Int->Int->Int->Point->Point
applyForce rootforce a b index force = 
    let 
      newforce:Point
      newforce = 
        case index of 
          a -> rootforce
          b -> invertPoint rootforce
          _ -> Point 0 0
    in (addPoints force newforce)

-- gets the forces applied to each point by the springs, returning
-- the list of springs
addSpringForce:Array PointMass->Spring->Array Point->Array Point
addSpringForce pointmasses spring forces = 
  let rootforce = (springForce spring pointmasses)
      a = spring.index_a
      b = spring.index_b
  in indexedMap (applyForce rootforce a b) forces

getSpringForces:Cloth->Array Point
getSpringForces cloth = foldl (addSpringForce cloth.pointmasses)
                        (filledArray (Point 0 0) (length cloth.pointmasses))
                        cloth.springs


resultingVelocity:PointMass->Point->Float->Point
resultingVelocity ptmass force time = 
  let adjustedVelocity = 
    scalePoint (time / (ptmass.mass)) force
  in addPoints
    ptmass.velocity
    adjustedVelocity

-- takes a cloth and updates the velocities of the points according to the
-- positions of the points / springs
updateClothVelocity:Array Point->Cloth->Float->Cloth
updateClothVelocity forces cloth time = 
  let newvels = indexedMap
        (\ index ptmass -> 
          resultingVelocity ptmass (forces !| index) time)
        cloth.pointmasses

      newPointMasses = indexedMap 
          (\index clothpoint->
            if (clothpoint.fixed)
              then clothpoint
              else {clothpoint |
                  velocity <- (newvels !| index)})
          cloth.pointmasses

  in Cloth newPointMasses cloth.springs

-- moves each of the pointmasses in the cloth by the appropriate distance
-- as given by their velocity
updateClothPosition:Cloth->Float->Cloth
updateClothPosition cloth dt =
  {cloth | pointmasses <-
    map (\ptmass ->
          {ptmass | 
            loc <- addPoints 
                      ptmass.loc 
                      (scalePoint dt ptmass.velocity)})
      cloth.pointmasses}


updateCloth:Cloth->Float->Cloth
updateCloth previous_cloth dt = 
  let 
    gravForces: Array Point
    gravForces = gravityArray (length previous_cloth.pointmasses)

    cloth_forces:Array Point
    cloth_forces = getSpringForces 
                      previous_cloth

    sum_forces = indexedMap 
                  (\i force -> addPoints force (gravForces !| i))
                  cloth_forces

    cloth_with_velocities:Cloth
    cloth_with_velocities = updateClothVelocity cloth_forces previous_cloth dt
  in updateClothPosition cloth_with_velocities dt


drawCloth : Cloth->Color->Form
drawCloth cloth color = 
  let ptdb:Array PointMass
      ptdb = cloth.pointmasses
      springs:Array Spring
      springs = cloth.springs
  in group
    ( (toList (map 
        (\ spring -> 
          let a = (cloth.pointmasses !| spring.index_a).loc
              b = (cloth.pointmasses !| spring.index_b).loc
          in traced 
              (solid color) 
              (path [(a.x, a.y), (b.x, b.y)]) )
        springs))
      ++ 
      (toList (map 
        (\ptms -> (circle 2) 
                    |> (if ptms.fixed 
                        then (filled color) 
                        else (outlined (solid color)))
                    |> (move (ptms.loc.x, ptms.loc.y)))
        ptdb))
    )

---------------------------- higher level logic stuff

-- create a rectangular piece of cloth
rectCloth:(Float, Float)->(Int,Int)->Float->Float->Float->Cloth
rectCloth (offset_x, offset_y) 
            (cloth_width, cloth_height) 
            point_mass
            spring_len spring_k = 
  let coords:Array (Int, Int)
      coords = 
        (map (\ n -> (n % cloth_width, n//cloth_width)) 
          (fromList [0 .. cloth_width * cloth_height - 1]))
  in Cloth 
      --generate the array of points
      (map 
        (\(x_int, y_int) -> 
          let x = toFloat x_int
              y = toFloat y_int
          in {default_pointmass | 
                loc <- (Point
                      (offset_x + x * spring_len)
                      (offset_y + y * spring_len))
                , fixed <- ( (x_int == 0 || x_int == cloth_width - 1) 
                          && (y_int == cloth_height - 1))
                , mass <- point_mass
              })
        coords)

      -- and the springs containing those points   
      (let  connections:Array (List (Spring))
            connections = 
              (indexedMap 
                (\ ind (x, y) -> 
                  (if (x /= (cloth_width - 1))
                    then [(Spring ind (ind+1) spring_k spring_len)]
                    else [])
                  ++
                  (if (y /= (cloth_height - 1))
                    then [(Spring ind (ind+cloth_width) spring_k spring_len)]
                    else []))
              coords)
      in fromList (foldl (++) [] connections))
        
      
