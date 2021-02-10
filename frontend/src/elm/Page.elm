module Page exposing
    ( Msg
    , Session(..)
    , errorToString
    , init
    , jsAddressToMsg
    , msgToString
    , subscriptions
    , toShared
    , update
    , viewGuest
    )

import Browser.Navigation as Nav
import Flags exposing (Flags)
import Html exposing (Attribute, Html, a, br, button, div, img, label, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, for, src, title, type_, value)
import Html.Events exposing (on)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Ports
import Profile exposing (Profile)
import Route exposing (Route)
import Session.Guest as Guest
import Session.Shared as Shared exposing (Shared)
import Time exposing (Posix)
import UpdateResult as UR
import Url exposing (Url)



-- INIT


init : Flags -> Nav.Key -> Url -> UpdateResult
init flags navKey url =
    let
        shared =
            Shared.init flags navKey url
    in
        let
            ( model, cmd ) =
                Guest.init shared
        in
            UR.init (Guest model)
                |> UR.addCmd (Cmd.map GotGuestMsg cmd)




-- SUBSCRIPTIONS


subscriptions : Session -> Sub Msg
subscriptions (Guest guest) =
    Guest.subscriptions guest
        |> Sub.map GotGuestMsg



-- MODEL


type Session
    = Guest Guest.Model



-- VIEW


viewGuest : (Msg -> msg) -> Guest.Page -> Guest.Model -> Html msg -> Html msg
viewGuest thisMsg page model content =
    Guest.view (thisMsg << GotGuestMsg) page model content


-- VIEW >> HELPERS


onClick : (a -> msg) -> Decoder a -> Html.Attribute msg
onClick toMsg decoder =
    on "click" (Decode.map toMsg decoder)


errorToString : Http.Error -> String
errorToString errorData =
    "Http Error"



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Session Msg ()


type Msg
    = GotGuestMsg Guest.Msg


update : Msg -> Session -> UpdateResult
update msg session =
    case ( msg, session ) of
        ( GotGuestMsg subMsg, Guest subModel ) ->
            Guest.update subMsg subModel
                |> UR.map Guest GotGuestMsg (\() uR -> uR)
                   


updateShared : Session -> (Shared -> Shared) -> Session
updateShared (Guest guest) transform =
    Guest { guest | shared = transform guest.shared }


-- INFO


toShared : Session -> Shared
toShared (Guest guest)  =
    guest.shared


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    Nothing


msgToString : Msg -> List String
msgToString (GotGuestMsg subMsg) =
    "GotGuestMsg" :: Guest.msgToString subMsg
