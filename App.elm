module App exposing (..)
{-| Main application entry point
-}
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
import ElmAbuse exposing (forceStyle)
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
    , params : Cloth.ClothParams
    }


initialParams : Cloth.ClothParams
initialParams =
    { offsets = Point 0 0
    , dimensions = ( 13, 6 )
    , point_mass = 0.1
    , spring_len = 20
    , spring_k = 3
    , damping_factor = 0.996
    , gravity = Point 0 -19.6
    , fixed_points = Nothing
    }


initialState : GameState
initialState =
    { cloth = Cloth.rectCloth initialParams
    , frame = { width = 800, height = 600 }
    , heldPointIndex = Nothing
    , heldPointWasFixed = Nothing
    , mousePosition = Point 0 0
    , params = initialParams
    }


type Msg
    = Tick Time
    | Resize Window.Size
    | MouseMove Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | AdjustParameter SimulationSetting
    | NoOp


type SimulationSetting
    = ClothWidth Int
    | ClothHeight Int
    | PointMass Float
    | SpringLen Float
    | SpringK Float
    | Damping Float
    | GravityX Float
    | GravityY Float


type alias SimulationSlider =
    { setting : Float -> SimulationSetting
    , label : String
    , min : Float
    , max : Float
    , step : Maybe Float
    }


interfaceDesc : List SimulationSlider
interfaceDesc =
    [ { setting = ClothWidth << floor
      , label = "width"
      , min = 1
      , max = 25
      , step = Just 1
      }
    , { setting = ClothHeight << floor
      , label = "height"
      , min = 1
      , max = 25
      , step = Just 1
      }
    , { setting = PointMass
      , label = "point mass"
      , min = 0
      , max = 2
      , step = Nothing
      }
    , { setting = SpringLen
      , label = "spring length"
      , min = 0
      , max = 100
      , step = Nothing
      }
    , { setting = SpringK
      , label = "spring k"
      , min = 0
      , max = 10
      , step = Nothing
      }
    , { setting = Damping
      , label = "damping factor"
      , min = 0
      , max = 1
      , step = Nothing
      }
    , { setting = GravityX
      , label = "gravity x"
      , min = -30
      , max = 30
      , step = Nothing
      }
    , { setting = GravityY
      , label = "gravity y"
      , min = -30
      , max = 30
      , step = Nothing
      }
    ]


adjustSimulationParams : SimulationSetting -> GameState -> GameState
adjustSimulationParams param state =
    let
        cloth =
            state.cloth

        params =
            state.params

        toParams state new_params =
            { state
                | cloth = Cloth.rectCloth new_params
                , heldPointIndex = Nothing
                , heldPointWasFixed = Nothing
                , params = new_params
            }
    in
        case param of
            ClothWidth w ->
                let
                    new_params =
                        { params | dimensions = ( w, snd params.dimensions ) }
                in
                    toParams state new_params

            ClothHeight h ->
                let
                    new_params =
                        { params | dimensions = ( fst params.dimensions, h ) }
                in
                    toParams state new_params

            PointMass mass ->
                let
                    new_params =
                        { params | point_mass = mass }
                in
                    toParams state new_params

            SpringLen l ->
                let
                    new_params =
                        { params | spring_len = l }
                in
                    toParams state new_params

            SpringK k ->
                let
                    new_params =
                        { params | spring_k = k }
                in
                    toParams state new_params

            Damping d ->
                { state
                    | cloth = { cloth | damping_factor = d }
                    , params = { params | damping_factor = d }
                }

            GravityX x ->
                { state
                    | cloth = { cloth | gravity = Point x state.cloth.gravity.y }
                    , params = { params | gravity = Point x state.params.gravity.y }
                }

            GravityY y ->
                { state
                    | cloth = { cloth | gravity = Point state.cloth.gravity.x y }
                    , params = { params | gravity = Point state.params.gravity.x y }
                }


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


getCurrent : (Float -> SimulationSetting) -> GameState -> Float
getCurrent constructor model =
    case (constructor 0) of
        ClothWidth _ ->
            toFloat <| fst model.params.dimensions

        ClothHeight _ ->
            toFloat <| snd model.params.dimensions

        PointMass _ ->
            model.params.point_mass

        SpringLen _ ->
            model.params.spring_len

        SpringK _ ->
            model.params.spring_k

        Damping _ ->
            model.cloth.damping_factor

        GravityX _ ->
            model.cloth.gravity.x

        GravityY _ ->
            model.cloth.gravity.y


viewSliders : GameState -> Html Msg
viewSliders model =
    let
        violentToFloat : String -> Float
        violentToFloat str =
            case String.toFloat str of
                Err e ->
                    0

                Ok v ->
                    v

        makeSlider : GameState -> SimulationSlider -> Html Msg
        makeSlider model slider =
            Html.div []
                [ Html.label [ Html.Attributes.name slider.label ]
                    [ Html.text slider.label ]
                , Html.input
                    [ type' "range"
                    , Html.Attributes.name <| slider.label
                    , Html.Attributes.min <| toString slider.min
                    , Html.Attributes.max <| toString slider.max
                    , Html.Attributes.step
                        <| case slider.step of
                            Just step ->
                                toString step

                            Nothing ->
                                "0.001"
                    , defaultValue <| toString <| getCurrent slider.setting model
                    , on "input"
                        (Json.map (AdjustParameter << slider.setting << violentToFloat)
                            (Json.at [ "target", "value" ] Json.string)
                        )
                    ]
                    []
                ]
    in
        Html.div [ id "parameters" ]
            [ Html.form [] (List.map (makeSlider model) interfaceDesc) ]



-- render gamestate at width and height


render : GameState -> Form
render state =
    Cloth.drawCloth state.cloth (rgb 217 0 0)


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
    let
        readSize =
            Task.perform (\size -> Resize size)
                (\size -> Resize size)
                Window.size

        injectStyles =
            Task.perform (\_ -> NoOp)
                (\_ -> NoOp)
                (forceStyle [ "style.css" ])
    in
        Cmd.batch [ injectStyles, readSize ]


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

        shouldGrab =
            case closest of
                Nothing ->
                    False

                Just ( index, pt ) ->
                    dist state.mousePosition pt.loc < 20
    in
        if shouldGrab then
            fixHeld
                { state
                    | heldPointIndex = closest_index
                    , heldPointWasFixed = is_closest_fixed
                }
        else
            state


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

        AdjustParameter param ->
            adjustSimulationParams param state

        NoOp ->
            state



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
