module Main exposing (main)

import AnimationFrame
import Hash
import Html exposing (Html, div, li, text, ul)
import Html.Attributes
import Random
import Svg exposing (Svg, circle, g, rect, svg)
import Svg.Attributes
import Time
import WebSocket
import Time exposing (Time)
import Json.Decode


type alias Model =
    { observations : List (Animated Observation)
    , config : Config
    , events : List String
    }


type alias Config =
    { width : Int
    , height : Int
    , baseRadius : Int
    }


duration : Time.Time
duration =
    Time.second * 3


type Msg
    = FrameDiff Time.Time
    | Event (Result String Observation)


type alias Observation =
    { meterId : Int
    , measurements : List Measurement
    }


type alias Measurement =
    { count : Int
    , duration : Time.Time
    }


type Animated a
    = Animating (AnimationSpec a)
    | Done a


type alias AnimationSpec a =
    { duration : Time.Time
    , elapsed : Time.Time
    , data : a
    }


newAnimation : Time.Time -> a -> Animated a
newAnimation duration a =
    Animating
        { duration = duration
        , elapsed = 0
        , data = a
        }


animationData : Animated a -> a
animationData animation =
    case animation of
        Done data ->
            data

        Animating spec ->
            spec.data


isFinished : Animated a -> Bool
isFinished animation =
    case animation of
        Done _ ->
            True

        Animating _ ->
            False


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    { observations = []
    , config =
        { height = 400
        , width = 700
        , baseRadius = 15
        }
    , events = []
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FrameDiff diff ->
            { model | observations = List.map (animationTick diff) model.observations } ! []

        Event result ->
            case result of
                Err err ->
                    { model | events = err :: model.events } ! []

                Ok observation ->
                    { model
                        | observations = (newAnimation duration observation) :: model.observations
                        , events = (toString observation) :: model.events
                    }
                        ! []


animationTick : Time.Time -> Animated a -> Animated a
animationTick diff animation =
    case animation of
        Done a ->
            animation

        Animating spec ->
            let
                newElapsed =
                    spec.elapsed + diff
            in
                if newElapsed > spec.duration then
                    Done spec.data
                else
                    Animating { spec | elapsed = newElapsed }


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style [ ( "margin", "5em auto" ), ( "width", "44em" ) ] ]
        [ plan model
        , history model.observations
        , messages model.events
        ]


plan : Model -> Html Msg
plan model =
    svg
        [ Svg.Attributes.width <| toString model.config.width
        , Svg.Attributes.height <| toString model.config.height
        ]
        [ rect
            [ Svg.Attributes.stroke "#666"
            , Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.fill "#eee"
            ]
            []
        , (bubbles model)
        ]


bubbles : Model -> Svg Msg
bubbles model =
    let
        fns =
            [ xFromMeterId model.config >> toString >> Svg.Attributes.cx
            , yFromMeterId model.config >> toString >> Svg.Attributes.cy
            , radius model.config >> toString >> Svg.Attributes.r
            ]
    in
        model.observations
            |> List.filter (not << isFinished)
            |> List.map (bubble fns)
            |> g []


xFromMeterId : Config -> Animated Observation -> Float
xFromMeterId config animation =
    let
        fn =
            (\observation ->
                Hash.hash (toString observation.meterId)
                    % (config.width - 2 * config.baseRadius)
                    + config.baseRadius
                    |> toFloat
            )
    in
        defaultDoneObservation fn animation


yFromMeterId : Config -> Animated Observation -> Float
yFromMeterId config animation =
    let
        fn =
            (\observation ->
                Hash.hash (toString observation.meterId)
                    % (config.height - 2 * config.baseRadius)
                    + config.baseRadius
                    |> toFloat
            )
    in
        defaultDoneObservation fn animation


defaultDoneObservation : (Observation -> Float) -> Animated Observation -> Float
defaultDoneObservation fn animation =
    case animation of
        Done _ ->
            -999999

        Animating spec ->
            fn spec.data


radius : Config -> Animated Observation -> Float
radius config animation =
    let
        coef =
            case animation of
                Done data ->
                    1.0

                Animating spec ->
                    if spec.elapsed == 0 then
                        0
                    else
                        spec.elapsed / spec.duration
    in
        (toFloat config.baseRadius * 2) * coef + (toFloat config.baseRadius)


animatedSvgAttributes : AnimationFns -> Animated Observation -> List (Svg.Attribute Msg)
animatedSvgAttributes fns animation =
    List.map (\fn -> fn animation) fns


type alias AnimationFn =
    Animated Observation -> Svg.Attribute Msg


type alias AnimationFns =
    List AnimationFn


bubble : AnimationFns -> Animated Observation -> Svg Msg
bubble animationFns observation =
    circle
        ([ Svg.Attributes.stroke "#338"
         , Svg.Attributes.fill "#66a"
         , Html.Attributes.style [ ( "opacity", ".6" ) ]
         ]
            ++ (animatedSvgAttributes animationFns observation)
        )
        []


history : List (Animated Observation) -> Html Msg
history observations =
    observations
        |> List.map (\o -> li [] [ text <| toString <| o ])
        |> ul []


messages : List String -> Html Msg
messages events =
    events
        |> List.map (\m -> li [] [ text m ])
        |> ul []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs FrameDiff
        , WebSocket.listen "ws://localhost:8001/random" decodeEvent
        ]


decodeEvent : String -> Msg
decodeEvent s =
    s
        |> Json.Decode.decodeString eventDecoder
        |> Event


eventDecoder : Json.Decode.Decoder Observation
eventDecoder =
    Json.Decode.map2 Observation
        (Json.Decode.field "meter_id" Json.Decode.int)
        (Json.Decode.field "measurements" (Json.Decode.list measurementDecoder))


measurementDecoder : Json.Decode.Decoder Measurement
measurementDecoder =
    Json.Decode.map2 Measurement
        (Json.Decode.field "count" Json.Decode.int)
        (Json.Decode.field "duration" Json.Decode.float)
