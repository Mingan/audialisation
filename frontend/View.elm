module View exposing (view)

import Html exposing (Html, div, li, text, ul)
import Html.Attributes
import Random
import Svg exposing (Svg, circle, g, rect, svg)
import Svg.Attributes
import Hash
import Types exposing (..)


type alias AnimationFn =
    Addition -> Svg.Attribute Msg


type alias AnimationFns =
    List AnimationFn


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "margin", "2em auto" )
            , ( "width", (toString model.config.width) ++ "px" )
            ]
        ]
        [ plan model
        , history model.additions
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
        model.additions
            |> List.filter (not << isFinished)
            |> List.map (bubble fns)
            |> g []


xFromMeterId : Config -> Addition -> Float
xFromMeterId =
    coordFromMeterId .width


yFromMeterId : Config -> Addition -> Float
yFromMeterId =
    coordFromMeterId .height


coordFromMeterId : (Config -> Int) -> Config -> Addition -> Float
coordFromMeterId fn config addition =
    Hash.hash (toString addition.meterId)
        % ((fn config) - 2 * config.baseRadius)
        + config.baseRadius
        |> toFloat


radius : Config -> Addition -> Float
radius config addition =
    let
        ( progress, size ) =
            ( 1.0, addition.count )
    in
        (toFloat (config.baseRadius * size)) * progress + (toFloat config.baseRadius)


bubble : AnimationFns -> Addition -> Svg Msg
bubble animationFns addition =
    circle
        ([ Svg.Attributes.stroke "#338"
         , Svg.Attributes.fill "#66a"
         , Html.Attributes.style [ ( "opacity", ".6" ) ]
         ]
            ++ (animatedSvgAttributes animationFns addition)
        )
        []


animatedSvgAttributes : AnimationFns -> Addition -> List (Svg.Attribute Msg)
animatedSvgAttributes fns animation =
    List.map (\fn -> fn animation) fns


history : List Addition -> Html Msg
history additions =
    additions
        |> List.map (\o -> li [] [ text <| toString <| o ])
        |> ul [ Html.Attributes.style [ ( "padding", "0px" ), ( "width", "62%" ), ( "margin-right", "2%" ), ( "float", "left" ) ] ]


messages : List String -> Html Msg
messages events =
    events
        |> List.map (\m -> li [] [ text m ])
        |> ul [ Html.Attributes.style [ ( "padding", "0px" ), ( "width", "35%" ), ( "float", "left" ) ] ]
