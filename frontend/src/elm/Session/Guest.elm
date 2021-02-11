module Session.Guest exposing (External(..), Model, Msg(..), Page(..), addAfterLoginRedirect, init, initModel, msgToString, subscriptions, update, view)

import Browser.Events
import Html exposing (Html, a, button, div, header, i, img, p, text)
import Html.Attributes exposing (alt, class, classList, src, style, tabindex, type_)
import Html.Events exposing (onClick, onMouseEnter)
import Http
import Json.Decode as Decode
import Ports
import Profile exposing (Profile)
import Route exposing (Route)
import Session.Shared as Shared exposing (Shared)
import UpdateResult as UR



-- INIT


init : Shared -> ( Model, Cmd Msg )
init shared =
    ( initModel shared
    , Cmd.none
    )



-- MODEL


type alias Model =
    { shared : Shared
    , showNav : Bool
    }


initModel : Shared -> Model
initModel shared =
    { shared = shared
    , showNav = False
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyDown (Browser.Events.onKeyDown (Decode.field "key" Decode.string))



-- VIEW


{-| Page types the Guest can access.
-}
type Page
    = Other
    | Home


view : (Msg -> msg) -> Page -> Model -> Html msg -> Html msg
view thisMsg page ({ shared } as model) content =
    div []
        [ viewPageHeader model shared
            |> Html.map thisMsg
        , content
        , viewFooter
        ]


viewPageHeader : Model -> Shared -> Html Msg
viewPageHeader model shared =
    let
        navVisibility =
            case model.showNav of
                False ->
                    "hidden"

                True ->
                    "block"
    in
    div
        []
        []


viewFooter : Html msg
viewFooter =
    div [ class "" ]
        []



-- UPDATE


type External
    = UpdatedGuest Model


type alias UpdateResult =
    UR.UpdateResult Model Msg ()


type Msg
    = KeyDown String
    | ToggleNavBar


update : Msg -> Model -> UpdateResult
update msg ({ shared } as model) =
    case msg of
        KeyDown key ->
            if key == "Esc" || key == "Escape" then
                model
                    |> UR.init

            else
                model
                    |> UR.init

        ToggleNavBar ->
            { model | showNav = not model.showNav }
                |> UR.init



-- TRANSFORM


addAfterLoginRedirect : Route -> Model -> Model
addAfterLoginRedirect _ model =
    model


msgToString : Msg -> List String
msgToString msg =
    case msg of
        KeyDown _ ->
            [ "KeyDown" ]

        ToggleNavBar ->
            [ "ToggleNavBar" ]
