module Cloth where
import Primitives(..)
import Utils(..)
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Time (inSeconds, fps, every, second)
import Signal
import Array(Array, map, indexedMap, foldl,
              length, get, fromList, empty, 
              toList, append)

type alias Spring = {index_a:Int, index_b:Int, k:Float, len:Float}
type alias Cloth = {pointmasses:Array PointMass, springs:Array Spring}

default_spring = {  index_a = -1,
                    index_b = -1,
                    k = 1.0,
                    len = 1.0}

-- returns the force the spring exterts on pointmass a
-- the force exerted on pointmass b is just the inverse of a.
springForce:Spring->Array PointMass->Point
springForce s pointdb = let aloc = (pointdb !| s.index_a).loc
                            bloc = (pointdb !| s.index_b).loc
                            realdistance = dist aloc bloc
                            rawforce = s.k * (s.len - realdistance)
                        in similarTri (diffPoints aloc bloc) rawforce

-- applies a force to the two points given by i
applyForce:Point->Int->Int->Int->Point->Point
applyForce root_force a b   index prev_force = 
   if index == a 
      then addPoints prev_force root_force
      else if index == b
        then addPoints prev_force (invertPoint root_force)
        else prev_force

-- gets the forces applied to each point by the springs, returning
-- the list of springs
addSpringForce:Array PointMass->Spring->Array Point->Array Point
addSpringForce pointmasses spring forces = 
  let rootforce = (springForce spring pointmasses)
      a = spring.index_a
      b = spring.index_b
  in indexedMap (applyForce rootforce a b) forces

springForces:Cloth->Array Point
springForces cloth = foldl (addSpringForce cloth.pointmasses)
                        (filledArray (Point 0 0) (length cloth.pointmasses))
                        cloth.springs


resultingVelocity:PointMass->Point->Float->Point
resultingVelocity ptmass force time = 
  let adjustedVelocity = 
    scalepoint (time / (ptmass.mass)) force
  in addPoints
    ptmass.velocity
    adjustedVelocity

-- takes a cloth and updates the velocities of the points according to the
-- positions of the points / springs
updateClothVelocity:Array Point->Cloth->Float->Cloth
updateClothVelocity forces cloth time = 
  let newvels = map
        (\ (force, ptmass) -> 
          resultingVelocity ptmass force time)
        (zip forces cloth.pointmasses)

      newPointMasses = map 
          (\ (new_velocity, clothpoint, force) ->
            if (clothpoint.fixed)
              then clothpoint
              else {clothpoint |
                  velocity <- new_velocity,
                  last_force <- force})
          (zip3 newvels cloth.pointmasses forces)

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
                      (scalepoint dt ptmass.velocity)})
      cloth.pointmasses}


updateCloth:Cloth->Float->Cloth
updateCloth previous_cloth dtime_millis = 
  let 
    -- at most a dt of 0.2 seconds to avoid explosion in laggy situations
    dt = min (dtime_millis / 1000) 0.2
    grav_forces: Array Point
    grav_forces = gravityArray (previous_cloth.pointmasses)

    cloth_forces:Array Point
    cloth_forces = springForces 
                      previous_cloth

    sum_forces = map 
                  (\ (g_force, c_force) -> addPoints c_force g_force)
                  (zip grav_forces cloth_forces)

    cloth_with_velocities:Cloth
    cloth_with_velocities = updateClothVelocity sum_forces previous_cloth dt
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
      {-++ 
      (toList (map 
        (\ptms -> 
          let dest = addPoints (ptms.loc) (ptms.last_force)
          in  traced
              (solid (inversecolor color))
              (path [ (ptms.loc.x, ptms.loc.y) 
                    , (dest.x, dest.y)]))
        ptdb))
      ++ 
      (toList (map 
        (\ptms -> 
          let dest = addPoints (ptms.loc) (ptms.velocity)
          in  traced
              (solid yellow)
              (path [ (ptms.loc.x, ptms.loc.y) 
                    , (dest.x, dest.y)]))
        ptdb))-}
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
        
      
