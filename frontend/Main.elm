module Main exposing (main)

import AnimationFrame
import Html exposing (Html, div, li, text, ul)
import Html.Attributes
import Random
import Svg exposing (Svg, circle, g, rect, svg)
import Svg.Attributes
import WebSocket
import Json.Decode
import Types exposing (..)
import View exposing (..)
import State exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs FrameDiff
        , WebSocket.listen "ws://localhost:8001/random" wsMessage
        ]


wsMessage : String -> Msg
wsMessage s =
    s
        |> Json.Decode.decodeString eventDecoder
        |> Event


eventDecoder : Json.Decode.Decoder Addition
eventDecoder =
    Json.Decode.map3 Addition
        (Json.Decode.field "meter_id" Json.Decode.int)
        (Json.Decode.field "count" Json.Decode.int)
        (Json.Decode.field "duration" Json.Decode.float)
