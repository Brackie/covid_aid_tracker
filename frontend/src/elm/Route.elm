module Route exposing (Route(..), fromUrl, href, pushUrl, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Builder exposing (QueryParameter, int)
import Url.Parser as Url exposing ((</>), (<?>), Parser, int, oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = Root
    | Home

parser : Url -> Parser (Route -> a) a
parser url =
    oneOf
        [ Url.map Root top
        , Url.map Home (s "home")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Url.parse (parser url) url


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)



-- INTERNAL


parseRedirect : Url -> Maybe String -> Maybe Route
parseRedirect url maybeQuery =
    let
        protocol =
            case url.protocol of
                Url.Http ->
                    "http://"

                Url.Https ->
                    "https://"

        host =
            url.host

        port_ =
            case url.port_ of
                Nothing ->
                    ""

                Just p ->
                    ":" ++ String.fromInt p
    in
    maybeQuery
        |> Maybe.andThen (\query -> Url.fromString (protocol ++ host ++ port_ ++ query))
        |> Maybe.andThen (\url_ -> Url.parse (parser url_) url_)


queryBuilder : (a -> String) -> Maybe a -> String -> List QueryParameter
queryBuilder fn maybeRedirect queryParam =
    Maybe.map fn maybeRedirect
        |> Maybe.map (Url.Builder.string queryParam)
        |> List.singleton
        |> List.filterMap identity


routeToString : Route -> String
routeToString route =
    let
        ( paths, queries ) =
            case route of
                Root ->
                    ( [], [] )

                Home ->
                    ( [ "home" ], [] )

    in
    Url.Builder.absolute paths queries
