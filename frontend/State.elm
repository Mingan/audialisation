port module State exposing (init, update)

import Types exposing (..)
import Time exposing (Time)


port playSound : Int -> Cmd msg


init : ( Model, Cmd Msg )
init =
    { additions = []
    , config =
        { height = 700
        , width = 1100
        , baseRadius = 15
        }
    , events = []
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FrameDiff diff ->
            updateFrameDiff model diff

        Event result ->
            updateWsEvent model result


updateFrameDiff : Model -> Time.Time -> ( Model, Cmd Msg )
updateFrameDiff model diff =
    { model | additions = List.map (animationTick diff) model.additions } ! []


updateWsEvent : Model -> Result String Addition -> ( Model, Cmd Msg )
updateWsEvent model result =
    case result of
        Err err ->
            { model | events = err :: model.events } ! []

        Ok addition ->
            let
                animation =
                    newAnimation (addition.duration / 1000 / 1000) addition
            in
                { model
                    | additions = animation :: model.additions
                    , events = (toString addition) :: model.events
                }
                    ! [ playSound addition.count ]


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
