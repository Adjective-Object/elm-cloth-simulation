import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import signal
import Array(Array, map, indexedMap, foldl, length, get, fromList, empty, toList)

-- cuz im lazy and dont want to unpack maybes in contexts I know are safe
(!|):(Array a)->Int->a
(!|) arr ind = case (get ind arr) of
                  Just x -> x
                  _ -> (List.head (toList arr))


type alias Point = {x:Float, y:Float}
type alias PointMass = {loc:Point, velocity:Point, mass:Float}
type alias Spring = {index_a:Int, index_b:Int, k:Float, len:Float}

type alias Cloth = {pointmasses:Array PointMass, springs:Array Spring}

addPoints:Point->Point->Point
addPoints a b = Point ((a.x)+(b.x)) ((a.y)+(b.y))

invertPoint:Point->Point
invertPoint p = Point -p.x -p.y

scalePoint:Float->Point->Point
scalePoint scale p = Point (scale*p.x) (scale*p.y)

gravityArray:Int->Array Point
gravityArray l = indexedMap (\ index n -> (Point 0 9.8)) (fromList [0..l])

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

applyforce:Point->Int->Point->Point
applyforce rootforce index force = 
    let newforce = 
        case index of 
          a -> rootforce
          b -> invertPoint rootforce
          _ -> Point 0 0
    in (addPoints force newforce)

-- applies forces to the 
addSpringForces:Array PointMass->Spring->Array Point->Array Point
addSpringForces pointmasses spring forces = 
  let rootforce = (springForce spring pointmasses)
      a = spring.index_a
      b = spring.index_b
  in indexedMap (applyforce rootforce) forces

resultingVelocity:PointMass->Point->Float->Point
resultingVelocity ptmass force time = 
  let adjustedVelocity = 
    scalePoint (time / (ptmass.mass)) force
  in addPoints
    ptmass.velocity
    adjustedVelocity

-- takes a cloth and updates the velocities of the points according to the
-- positions of the points / springs
applyForces:Array Point->Cloth->Float->Cloth
applyForces forces cloth time = 
  let newvels = indexedMap 
        (\ index ptmass -> 
          resultingVelocity ptmass (forces !| index) time)
        cloth.pointmasses

      newPointMasses = indexedMap 
          (\index clothpoint->
            PointMass 
              clothpoint.loc
              (newvels !| index)
              clothpoint.mass)
          cloth.pointmasses

  in Cloth newPointMasses cloth.springs


-- returns a new cloth which is the input cloth,
-- with velocity updated (applies the wind and the gravity)
updateCloth:Cloth->Float->Cloth
updateCloth cloth time =
      let forces = foldl
                (addSpringForces cloth.pointmasses)
                (gravityArray (length cloth.springs))
                cloth.springs
      in applyForces forces cloth time


drawCloth : Array Point->Array Spring->Color->List Form
drawCloth ptdb springs color = 
  collage 300 300
    (List.map 
      (\ spring -> 
        let a = (ptdb !| spring.index_a)
            b = (ptdb !| spring.index_a)
        in traced 
            (solid color) 
            (path [(a.x, a.y) (b.x, b.y)])) springs)

---------------------------- mainloop stuff

rectCloth:(Int,Int)->(Int,Int)->Float->Float->Float->Cloth
rectCloth (offset_x, offset_y) 
            (cloth_width, cloth_height) 
            point_mass
            spring_len spring_k = 
  let coords = 
        (map (\ n -> 
          (n%cloth_width, n/cloth_width)) 
          (fromList [0 .. cloth_width * cloth_height]))
  in Cloth 
      --generate the list of points
      (map 
        (\(x, y) -> 
          PointMass (Point
                    offset_x + x * spring_len,
                    offset_y + y * spring_len)
          (Point 0 0),
          point_mass)
        coords
      )
    
      (indexedMap 
        (\ ind (x, y) -> 
            (if (x /= (cloth_width - 1))
              then [(Spring ind (ind+1 spring_k), spring_len)]
              else [])
            ++
            (if (y /= cloth_height - 1)
              then [(Spring ind (ind+cloth_width) spring_k, spring_len)]
              else [])
                     
          )
        coords)
      

maincloth = rectCloth (10, 10) (2, 3) 1 5 1

step dt cloth =
  updateCloth maincloth dt

mainclothfig : List Form
mainclothfig = drawCloth maincloth

main : Signal Element
main = Signal.map inSeconds (fps 30)
        |> Signal.foldp step maincloth
        |> Signal.map view


clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6

