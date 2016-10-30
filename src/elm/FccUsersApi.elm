module FccUsersApi exposing (..)

import Json.Encode
import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))

type alias FccUser =
    { username : String
    , img : String
    , alltime : Int
    , recent : Int
    , lastUpdate : String
    }

decodeFccUsers : Json.Decode.Decoder (List FccUser)
decodeFccUsers =
    Json.Decode.list decodeFccUser

decodeFccUser : Json.Decode.Decoder FccUser
decodeFccUser =
    Json.Decode.succeed FccUser
        |: ("username" := Json.Decode.string)
        |: ("img" := Json.Decode.string)
        |: ("alltime" := Json.Decode.int)
        |: ("recent" := Json.Decode.int)
        |: ("lastUpdate" := Json.Decode.string)


