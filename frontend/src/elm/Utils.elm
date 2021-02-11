module Utils exposing (decodeDate, decodeEnterKeyDown, decodeTimestamp)

import Iso8601
import Json.Decode as Decode exposing (Decoder, string)
import Time exposing (Posix)


decodeDate : Decoder Posix
decodeDate =
    string
        |> Decode.andThen
            (\s ->
                let
                    dateStr =
                        if String.endsWith "Z" s then
                            s

                        else
                            s ++ "Z"
                in
                case Iso8601.toTime dateStr of
                    Ok posix ->
                        Decode.succeed posix

                    Err _ ->
                        Decode.fail "Failed to parse date"
            )


decodeTimestamp : Decode.Decoder Time.Posix
decodeTimestamp =
    Decode.int
        |> Decode.andThen
            (\ms ->
                Decode.succeed <| Time.millisToPosix ms
            )


decodeEnterKeyDown : Decode.Decoder Bool
decodeEnterKeyDown =
    let
        isEnter code =
            case code of
                "Enter" ->
                    True

                _ ->
                    False
    in
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\cd ->
                Decode.succeed <| isEnter cd
            )
