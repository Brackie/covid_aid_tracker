module Funding exposing (..)

import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, Value, int, list, map3, null, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Time exposing (Posix)


type alias Funding =
    { amount : Int
    , source : String
    , recepient : String
    , counties : List String
    , date : Posix
    }


type Msg
    = GotFundingData (Result Http.Error (List Funding))


getFundingData : Cmd Msg
getFundingData =
    Http.get
        { url = "https://actionfortransparency.org/wp-content/plugins/a4t-covid-19/assets/funding.json"
        , expect = Http.expectJson GotFundingData (list fundingDecoder)
        }



-- helpers


inKindDonation : Funding -> Bool
inKindDonation funding =
    funding.amount == 0



-- decoders


amountDecoder : Decoder Int
amountDecoder =
    oneOf [ int, succeed 0 ]


countiesDecoder : Decoder (List String)
countiesDecoder =
    list string


fundingDecoder : Decoder Funding
fundingDecoder =
    Decode.succeed Funding
        |> required "amount" amountDecoder
        |> required "donor" string
        |> required "recepient" string
        |> required "counties" countiesDecoder
        |> required "date" Iso8601.decoder
