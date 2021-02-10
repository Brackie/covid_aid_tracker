module Profile exposing (Profile)

import Html exposing (..)
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode



-- Profile


type alias Profile =
    { email : String
    , token : String
    }
