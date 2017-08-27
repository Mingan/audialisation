module Types exposing (..)

import Time


type Msg
    = FrameDiff Time.Time
    | Event (Result String Addition)


type alias Model =
    { additions : List (Animated Addition)
    , config : Config
    , events : List String
    }


type alias Config =
    { width : Int
    , height : Int
    , baseRadius : Int
    }


type alias Addition =
    { meterId : Int
    , count : Int
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
