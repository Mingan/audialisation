module Main exposing (main)

import AnimationFrame
import Hash
import Html exposing (Html, div, li, text, ul)
import Html.Attributes
import Random
import Svg exposing (Svg, circle, g, rect, svg)
import Svg.Attributes
import Time


type alias Model =
    { observations : List (Animated Observation)
    , config : Config
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
    = Tick Time.Time
    | GeneratedMeterId Int
    | FrameDiff Time.Time


type alias Observation =
    { meterId : Int
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
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            model ! [ generateMeterId ]

        GeneratedMeterId id ->
            { model | observations = (newAnimation duration (Observation id)) :: model.observations } ! []

        FrameDiff diff ->
            { model | observations = List.map (animationTick diff) model.observations } ! []


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


generateMeterId : Cmd Msg
generateMeterId =
    Random.generate GeneratedMeterId (Random.int 0 2000)


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style [ ( "margin", "5em auto" ), ( "width", "44em" ) ] ]
        [ plan model
        , history model
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
    model.observations
        |> List.filter (isFinished >> not)
        |> List.map (bubble (xFromMeterId model.config) (yFromMeterId model.config) (radius model.config) model.config)
        |> g []


xFromMeterId : Config -> Observation -> Float
xFromMeterId config observation =
    Hash.hash (toString observation.meterId)
        % (config.width - 2 * config.baseRadius)
        + config.baseRadius
        |> toFloat


yFromMeterId : Config -> Observation -> Float
yFromMeterId config observation =
    Hash.hash (toString observation.meterId)
        % (config.height - 2 * config.baseRadius)
        + config.baseRadius
        |> toFloat


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


bubble : (Observation -> Float) -> (Observation -> Float) -> (Animated Observation -> Float) -> Config -> Animated Observation -> Svg Msg
bubble x y radius config observation =
    let
        data =
            animationData observation
    in
        circle
            [ Svg.Attributes.stroke "#338"
            , Svg.Attributes.fill "#66a"
            , Svg.Attributes.cx <| toString <| x data
            , Svg.Attributes.cy <| toString <| y data
            , Svg.Attributes.r <| toString <| radius observation
            , Html.Attributes.style [ ( "opacity", ".6" ) ]
            ]
            []


history : Model -> Html Msg
history model =
    model.observations
        |> List.map (\o -> li [] [ text <| toString <| o ])
        |> ul []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.millisecond * 333) Tick
        , AnimationFrame.diffs FrameDiff
        ]
