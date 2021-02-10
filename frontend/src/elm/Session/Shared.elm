module Session.Shared exposing
    ( Shared
    , init
    , viewFullError
    , viewFullLoading
    )

import Browser.Navigation as Nav
import Flags exposing (Endpoints, Environment, Flags)
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Time exposing (Posix)
import Url exposing (Url)


type alias Shared =
    { navKey : Nav.Key
    , environment : Environment
    , maybeToken : Maybe String
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Posix
    , url : Url
    }


init : Flags -> Nav.Key -> Url -> Shared
init ({ environment, maybeToken, endpoints } as flags) navKey url =
    { navKey = navKey
    , environment = environment
    , maybeToken = maybeToken
    , endpoints = endpoints
    , logo = flags.logo
    , logoMobile = flags.logoMobile
    , now = Time.millisToPosix flags.now
    , url = url
    }



-- VIEW


viewFullLoading : Html msg
viewFullLoading =
    div [ class "full-page-loading full-spinner-container" ]
        [ div [ class "spinner" ] [] ]


viewFullError : Shared -> Http.Error -> msg -> String -> Html msg
viewFullError shared _ msg msgText =
    div [ class "full-page-loading full-spinner-container" ]
        [ p [] [ text msgText ]
        , button [ onClick msg ] [ text "Try again" ]
        ]
