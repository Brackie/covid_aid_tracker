module Expenditure exposing (..)

import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, Value, int, list, map3, null, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Time exposing (Posix)


type alias Expenditure =
    { amount : Int
    , source : String
    , expendingBody : String
    , expenditureTypes : List String
    , date : Posix
    }


type Msg
    = GotExpenditureData (Result Http.Error (List Expenditure))


getExpenditureData : Cmd Msg
getExpenditureData =
    Http.get
        { url = "https://actionfortransparency.org/wp-content/plugins/a4t-covid-19/assets/expenditure.json"
        , expect = Http.expectJson GotExpenditureData (list expenditureDecoder)
        }



-- decoders


expenditureDecoder : Decoder Expenditure
expenditureDecoder =
    Decode.succeed Expenditure
        |> required "amount" int
        |> required "fund_source" string
        |> required "expending_body" string
        |> required "expenditure _types" (list string)
        |> required "period" Iso8601.decoder
