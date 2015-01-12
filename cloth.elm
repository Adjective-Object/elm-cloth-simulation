import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Time (inSeconds, fps)
import Signal
import Array(Array, map, indexedMap, foldl, length, get, fromList, empty, toList, append)

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

-- applies forces to the 
addSpringForces:Array PointMass->Spring->Array Point->Array Point
addSpringForces pointmasses spring forces = 
  let rootforce = (springForce spring pointmasses)
      a = spring.index_a
      b = spring.index_b
  in indexedMap (applyForce rootforce a b) forces

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
            {clothpoint |
                velocity <- (newvels !| index)})
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


drawCloth : Cloth->Color->Form
drawCloth cloth color = 
  let ptdb:Array Point
      ptdb = map (\ ptmass -> ptmass.loc) cloth.pointmasses
      springs:Array Spring
      springs = cloth.springs
  in group
    ( (toList (map 
        (\ spring -> 
          let a = (ptdb !| spring.index_a)
              b = (ptdb !| spring.index_b)
          in traced 
              (solid color) 
              (path [(a.x, a.y), (b.x, b.y)]) )
        springs))
      ++ 
      (toList (map 
        (\pt -> (circle 2) 
                    |> filled color
                    |> (move (pt.x, pt.y)))
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
        
      

maincloth = rectCloth (0, 0) (3, 4) 1 10 1

step dt cloth =
  updateCloth maincloth dt

view : Cloth->Element
view maincloth = 
  collage 600 600
    [ rect 300 300
            |> filled (rgb 46 9 39)
    , drawCloth maincloth (rgb 217 0 0)
    ]

main : Signal Element
main = Signal.map inSeconds (fps 30)
        |> Signal.foldp step maincloth
        |> Signal.map view


clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6

