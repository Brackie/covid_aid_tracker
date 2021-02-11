module Flags exposing
    ( Endpoints
    , Environment(..)
    , Flags
    , decode
    , default
    , defaultEndpoints
    )

import Json.Decode as Decode exposing (Decoder, nullable, string)
import Json.Decode.Pipeline as Decode exposing (optional, required)


type alias Flags =
    { environment : Environment
    , maybeToken : Maybe String
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Int
    }


default : Flags
default =
    { environment = Development
    , maybeToken = Nothing
    , endpoints = defaultEndpoints
    , logo = ""
    , logoMobile = ""
    , now = 0
    }


decode : Decoder Flags
decode =
    Decode.succeed Flags
        |> required "env" decodeEnvironment
        |> optional
            "account"
            (nullable string)
            Nothing
        |> required "endpoints" decodeEndpoints
        |> required "logo" Decode.string
        |> required "logoMobile" Decode.string
        |> required "now" Decode.int


type alias Endpoints =
    { graphql : String
    }


defaultEndpoints : Endpoints
defaultEndpoints =
    { graphql = "http://localhost:9098/graphql"
    }


decodeEndpoints : Decoder Endpoints
decodeEndpoints =
    Decode.succeed Endpoints
        |> required "graphql" string


type Environment
    = Development
    | Production


decodeEnvironment : Decoder Environment
decodeEnvironment =
    Decode.map
        (\env ->
            if env == "development" then
                Development

            else
                Production
        )
        string
