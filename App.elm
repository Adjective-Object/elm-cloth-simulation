module Main exposing (..)

-- where

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import List
import Window
import Text
import Task
import Html exposing (Html)
import Html.Attributes exposing (type', defaultValue, id, max, min)
import Html.Events exposing (on, targetValue)
import Html.App exposing (program)
import AnimationFrame exposing (..)
import Time exposing (Time)
import String
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
import Cloth
import Platform.Cmd
import Primitives exposing (Point, dist)
import Utils exposing (indexArray, (!|))
import Mouse
import Debug
import Json.Decode as Json


-- define the gamestate


type alias GameState =
    { cloth : Cloth.Cloth
    , frame : { width : Int, height : Int }
    , heldPointIndex : Maybe Int
    , heldPointWasFixed : Maybe Bool
    , mousePosition : Point
    }


initialState : GameState
initialState =
    { cloth =
        Cloth.rectCloth
            { offsets = ( 0, 0 )
            , dimensions = ( 13, 6 )
            , point_mass = 0.1
            , spring_len = 20
            , spring_k = 3
            , damping_factor = 0.96
            , gravity = Point 0 -19.6
            , fixed_points = [ ( 0, 5 ), ( 12, 5 ) ]
            }
    , frame = { width = 800, height = 600 }
    , heldPointIndex = Nothing
    , heldPointWasFixed = Nothing
    , mousePosition = Point 0 0
    }


type Msg
    = Tick Time
    | Resize Window.Size
    | MouseMove Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | AdjustGravity Float



-- render gamestate at width and height


render : GameState -> Form
render state =
    Cloth.drawCloth state.cloth (rgb 217 0 0)


viewCollage : GameState -> Html Msg
viewCollage model =
    collage model.frame.width
        model.frame.height
        [ rect (toFloat model.frame.width)
            (toFloat model.frame.height)
            |> filled black
        , render model
        , text
            (Text.fromString "THIS IS A GAME"
                |> Text.color red
            )
        ]
        |> Element.toHtml


viewSliders : GameState -> Html Msg
viewSliders model =
    let
        decodeStringToParam str =
            case String.toFloat str of
                Err e ->
                    AdjustGravity 0

                Ok v ->
                    AdjustGravity v
    in
        Html.div [ id "parameters" ]
            [ Html.form []
                [ Html.input
                    [ type' "range"
                    , Html.Attributes.min "-100"
                    , Html.Attributes.max "100"
                    , defaultValue <| toString model.cloth.gravity.y
                    , on "input"
                        (Json.map decodeStringToParam
                            (Json.at [ "target", "value" ] Json.string)
                        )
                    ]
                    []
                ]
            ]


view : GameState -> Html Msg
view model =
    Html.div []
        [ viewCollage model
        , viewSliders model
        ]


subscriptions : GameState -> Sub Msg
subscriptions state =
    Sub.batch
        [ diffs Tick
        , Window.resizes Resize
        , Mouse.moves MouseMove
        , Mouse.downs MouseDown
        , Mouse.ups MouseUp
        ]


fireInit =
    Task.perform (\size -> Resize size)
        (\size -> Resize size)
        Window.size


tup : Point -> ( Float, Float )
tup pt =
    ( pt.x, pt.y )


grabPoint : GameState -> GameState
grabPoint state =
    let
        findClosest ( new_ind, new_pt ) current =
            case current of
                Nothing ->
                    Just ( new_ind, new_pt )

                Just ( old_ind, old_pt ) ->
                    if
                        (dist state.mousePosition old_pt.loc
                            < dist state.mousePosition new_pt.loc
                        )
                    then
                        Just ( old_ind, old_pt )
                    else
                        Just ( new_ind, new_pt )

        closest =
            foldl findClosest Nothing <| indexArray state.cloth.pointmasses

        closest_index =
            case closest of
                Nothing ->
                    Nothing

                Just ( index, pt ) ->
                    Just index

        is_closest_fixed =
            case closest of
                Nothing ->
                    Nothing

                Just ( index, pt ) ->
                    Just pt.fixed
    in
        fixHeld
            { state
                | heldPointIndex = closest_index
                , heldPointWasFixed = is_closest_fixed
            }


applyToHeld state fn =
    let
        cloth =
            state.cloth
    in
        case state.heldPointIndex of
            Just index ->
                { state
                    | cloth =
                        { cloth
                            | pointmasses =
                                let
                                    oldPt =
                                        state.cloth.pointmasses !| index
                                in
                                    Array.set index (fn oldPt) state.cloth.pointmasses
                        }
                }

            Nothing ->
                state


fixHeld state =
    applyToHeld state (\a -> { a | fixed = True })


releasePoint state =
    let
        isNewPointFixed =
            case state.heldPointWasFixed of
                Just is_fixed ->
                    is_fixed

                Nothing ->
                    False

        state_ptfixed =
            applyToHeld state
                (\pt -> { pt | fixed = isNewPointFixed })
    in
        { state_ptfixed | heldPointIndex = Nothing }


movePoint : GameState -> Point -> GameState
movePoint state pt =
    let
        st =
            applyToHeld state (\a -> { a | loc = pt })
    in
        { st | mousePosition = pt }


step : Msg -> GameState -> GameState
step msg state =
    case msg of
        Tick dt ->
            { state | cloth = Cloth.updateCloth state.cloth dt }

        Resize size ->
            { state | frame = size }

        MouseDown pos ->
            movePoint (grabPoint state) state.mousePosition

        MouseUp pos ->
            releasePoint state

        MouseMove coords ->
            case state.heldPointIndex of
                Nothing ->
                    { state | mousePosition = screenToCanvas state coords }

                Just ind ->
                    movePoint state (screenToCanvas state coords)

        AdjustGravity grav ->
            let
                cloth =
                    state.cloth

                newGrav =
                    log { x = cloth.gravity.x, y = grav }

                newCloth =
                    { cloth | gravity = newGrav }
            in
                { state | cloth = newCloth }



--_ -> state


screenToCanvas : GameState -> Mouse.Position -> Point
screenToCanvas state pos =
    Point ((toFloat pos.x) - (toFloat state.frame.width) / 2)
        (0 - (toFloat pos.y) + (toFloat state.frame.height) / 2)


log : a -> a
log a =
    Debug.log (toString a) a


main =
    program
        { init = ( initialState, fireInit )
        , update = (\msg model -> ( step msg model, Cmd.none ))
        , subscriptions = subscriptions
        , view = view
        }
