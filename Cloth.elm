module Cloth exposing (..)

-- comment for synax highlighting where

import Primitives exposing (..)
import Utils exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import List
import Time exposing (second)
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
import Debug exposing (log, crash)


type alias Spring =
    { index_a : Int, index_b : Int, k : Float, len : Float }


type alias Cloth =
    { pointmasses : Array PointMass
    , springs : Array Spring
    , damping_factor : Float
    , gravity : Point
    }


default_spring =
    { index_a = -1
    , index_b = -1
    , k = 1.0
    , len = 1.0
    }


default_cloth =
    (Cloth (fromList []) (fromList []) 0.99 (Point 0 -9.8))



-- returns the force the spring exterts on pointmass a
-- the force exerted on pointmass b is just the inverse of a.


springForce : Spring -> Array PointMass -> Point
springForce s pointdb =
    let
        aloc =
            (pointdb !| s.index_a).loc

        bloc =
            (pointdb !| s.index_b).loc

        realdistance =
            dist aloc bloc

        rawforce =
            s.k * (s.len - realdistance)
    in
        similarTri (diffPoints aloc bloc) rawforce



-- applies a force to the two points given by i


applyForce : Point -> Int -> Int -> Int -> Point -> Point
applyForce root_force a b index prev_force =
    if index == a then
        addPoints prev_force root_force
    else if index == b then
        addPoints prev_force (invertPoint root_force)
    else
        prev_force



-- gets the forces applied to each point by the springs, returning
-- the list of springs


addSpringForce : Array PointMass -> Spring -> Array Point -> Array Point
addSpringForce pointmasses spring forces =
    let
        rootforce =
            (springForce spring pointmasses)

        a =
            spring.index_a

        b =
            spring.index_b
    in
        indexedMap (applyForce rootforce a b) forces


springForces : Cloth -> Array Point
springForces cloth =
    foldl (addSpringForce cloth.pointmasses)
        (filledArray (Point 0 0) (length cloth.pointmasses))
        cloth.springs


resultingVelocity : PointMass -> Point -> Float -> Point
resultingVelocity ptmass force time =
    let
        adjustedVelocity =
            scalePoint (time / (ptmass.mass)) force
    in
        addPoints ptmass.velocity
            adjustedVelocity



-- takes a cloth and updates the velocities of the points according to the
-- positions of the points / springs


updateClothVelocity : Array Point -> Cloth -> Float -> Cloth
updateClothVelocity forces cloth time =
    let
        newvels =
            map
                (\( force, ptmass ) ->
                    resultingVelocity ptmass force time
                        `addPoints` (scalePoint time cloth.gravity)
                )
                (zip forces cloth.pointmasses)

        newPointMasses =
            map
                (\( new_velocity, clothpoint, force ) ->
                    if (clothpoint.fixed) then
                        clothpoint
                    else
                        { clothpoint
                            | velocity = scalePoint (cloth.damping_factor) new_velocity
                            , last_force = force
                        }
                )
                (zip3 newvels cloth.pointmasses forces)
    in
        { cloth
            | pointmasses = newPointMasses
            , springs = cloth.springs
        }



-- moves each of the pointmasses in the cloth by the appropriate distance
-- as given by their velocity


updateClothPosition : Cloth -> Float -> Cloth
updateClothPosition cloth dt =
    { cloth
        | pointmasses =
            map
                (\ptmass ->
                    if ptmass.fixed then
                        ptmass
                    else
                        { ptmass
                            | loc =
                                addPoints ptmass.loc
                                    (scalePoint dt ptmass.velocity)
                        }
                )
                cloth.pointmasses
    }


updateCloth : Cloth -> Float -> Cloth
updateCloth previous_cloth dtime_millis =
    let
        -- at most a dt of 0.2 seconds to avoid explosion in laggy situations
        dt =
            (*) 2 <| min (dtime_millis / 500) 0.04

        cloth_forces : Array Point
        cloth_forces =
            springForces previous_cloth

        cloth_with_velocities : Cloth
        cloth_with_velocities =
            updateClothVelocity cloth_forces previous_cloth dt
    in
        updateClothPosition cloth_with_velocities dt


drawCloth : Cloth -> Color -> Form
drawCloth cloth color =
    let
        ptdb : Array PointMass
        ptdb =
            cloth.pointmasses

        springs : Array Spring
        springs =
            cloth.springs
    in
        group
            ((toList
                (map
                    (\spring ->
                        let
                            a =
                                (cloth.pointmasses !| spring.index_a).loc

                            b =
                                (cloth.pointmasses !| spring.index_b).loc
                        in
                            traced (solid color)
                                (path [ ( a.x, a.y ), ( b.x, b.y ) ])
                    )
                    springs
                )
             )
                ++ (toList
                        (map
                            (\ptms ->
                                (circle 2)
                                    |> (if ptms.fixed then
                                            (filled color)
                                        else
                                            (outlined (solid color))
                                       )
                                    |> (move ( ptms.loc.x, ptms.loc.y ))
                            )
                            ptdb
                        )
                   )
             --++
             --(toList (map
             --  (\ptms ->
             --    let dest = addPoints (ptms.loc) (ptms.last_force)
             --    in  traced
             --        (solid (complement color))
             --        (path [ (ptms.loc.x, ptms.loc.y)
             --              , (dest.x, dest.y)]))
             --  ptdb))
             --++
             --(toList (map
             --  (\ptms ->
             --    let dest = addPoints (ptms.loc) (ptms.velocity)
             --    in  traced
             --        (solid yellow)
             --        (path [ (ptms.loc.x, ptms.loc.y)
             --              , (dest.x, dest.y)]))
             --  ptdb))
            )



---------------------------- higher level logic stuff


type alias ClothParams =
    { offsets : ( Float, Float )
    , dimensions : ( Int, Int )
    , point_mass : Float
    , spring_len : Float
    , spring_k : Float
    , damping_factor : Float
    , gravity : Point
    , fixed_points : List ( Int, Int )
    }



-- create a rectangular piece of cloth


rectCloth : ClothParams -> Cloth
rectCloth params =
    let
        ( offset_x, offset_y ) =
            params.offsets

        ( cloth_width, cloth_height ) =
            params.dimensions

        point_mass =
            params.point_mass

        spring_len =
            params.spring_len

        spring_k =
            params.spring_k

        damping_factor =
            params.damping_factor

        gravity =
            params.gravity

        coords : Array ( Int, Int )
        coords =
            log "coords"
                <| (map (\n -> ( n % cloth_width, n // cloth_width ))
                        (fromList [0..cloth_width * cloth_height - 1])
                   )

        x_origin =
            (offset_x - toFloat cloth_width * spring_len) / 2

        y_origin =
            (offset_y - toFloat cloth_height * spring_len) / 2
    in
        { default_cloth
            | --generate the array of points
              pointmasses =
                (map
                    (\( x_int, y_int ) ->
                        let
                            x =
                                toFloat x_int

                            y =
                                toFloat y_int
                        in
                            { default_pointmass
                                | loc =
                                    (Point (x_origin + x * spring_len)
                                        (y_origin + y * spring_len)
                                    )
                                , fixed = ( x_int, y_int ) `List.member` params.fixed_points
                                , mass = point_mass
                            }
                    )
                    coords
                )
                -- and the springs containing those points
            , springs =
                (let
                    diag_dist : Float
                    diag_dist =
                        (dist origin (Point spring_len spring_len))

                    connections : Array (List (Spring))
                    connections =
                        (indexedMap
                            (\ind ( x, y ) ->
                                -- if its not on the right edge, connect too next
                                (if (x /= (cloth_width - 1)) then
                                    [ (Spring ind (ind + 1) spring_k spring_len) ]
                                 else
                                    []
                                )
                                    ++ -- if not on the bottom, connect to one lower
                                       (if (y /= (cloth_height - 1)) then
                                            [ (Spring ind (ind + cloth_width) spring_k spring_len) ]
                                        else
                                            []
                                       )
                             {-
                                ++
                                (if (y == 0)
                                  then [(Spring ind (length coords) spring_k spring_len)]
                                  else [])
                                ++
                                (if ((y /= cloth_height - 1) && (x /= cloth_width - 1))
                                  then [(Spring ind (ind+cloth_width+1) (spring_k/50) diag_dist)]
                                  else [])
                                ++
                                (if ((y /= cloth_height - 1) && (x /= 0))
                                  then [(Spring ind (ind+cloth_width-1) (spring_k/50) diag_dist)]
                                  else [])
                             -}
                            )
                            coords
                        )
                 in
                    fromList (foldl (++) [] connections)
                )
            , damping_factor = damping_factor
            , gravity = gravity
        }
