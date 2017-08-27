module Types exposing (..)

import Time


type Msg
    = FrameDiff Time.Time
    | Event (Result String Addition)


type alias Model =
    { additions : List Addition
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


isFinished : Addition -> Bool
isFinished addition =
    False
