module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Flags
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Log
import Page exposing (Session)
import Page.Home as Home exposing (..)
import Ports
import Route exposing (Route)
import Session.Guest as Guest
import UpdateResult as UR exposing (UpdateResult)
import Url exposing (Url)


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- INIT


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url navKey =
    let
        ( session, pageCmd ) =
            case Decode.decodeValue Flags.decode flagsValue of
                Ok flags ->
                    Page.init flags navKey url
                        |> UR.map identity GotPageMsg (\_ uR -> uR)
                        |> UR.toModelCmd (\_ m -> ( m, Cmd.none )) msgToString

                Err e ->
                    Page.init Flags.default navKey url
                        |> UR.map identity GotPageMsg (\_ uR -> uR)
                        |> UR.logDecodeError Ignored e
                        |> UR.toModelCmd (\_ m -> ( m, Cmd.none )) msgToString

        ( model, routeCmd ) =
            changeRouteTo (Route.fromUrl url)
                { session = session
                , status = Redirect
                }
    in
    ( model
    , Cmd.batch
        [ pageCmd
        , routeCmd
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotPageMsg (Page.subscriptions model.session)
        ]



-- MODEL


type alias Model =
    { session : Session
    , status : Status
    }


type Status
    = Redirect
    | NotFound
    | Home Home.Model



-- UPDATE


type Msg
    = Ignored
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotPageMsg Page.Msg
    | GotHomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        withGuest fn =
            case model.session of
                Page.Guest guest ->
                    fn guest

    in
    case ( msg, model.status ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model |> hideFeedback
                    , Nav.pushUrl (.navKey (Page.toShared model.session)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model |> hideFeedback
                    , Nav.load href
                    )

        ( GotPageMsg subMsg, _ ) ->
            Page.update subMsg model.session
                |> UR.map
                    (\s -> { model | session = s })
                    GotPageMsg
                    (\extMsg uR -> UR.addExt extMsg uR)
                |> UR.toModelCmd
                    (\extMsg m ->
                        (m, Cmd.none)
                    )
                    msgToString

        ( GotHomeMsg subMsg, Home subModel ) ->
            case model.session of
                Page.Guest _ ->
                    Home.update subMsg subModel
                        >> updateGuestUResult Home GotHomeMsg model
                        |> withGuest


        ( _, _ ) ->
            ( model
            , Log.impossible ("Main" :: msgToString msg |> String.join ".")
            )


updateStatusWith : (subModel -> Status) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateStatusWith toStatus toMsg model ( subModel, subCmd ) =
    ( { model | status = toStatus subModel }
    , Cmd.map toMsg subCmd
    )


updateSessionWith : (subMsg -> Msg) -> Model -> ( Session, Cmd subMsg ) -> ( Model, Cmd Msg )
updateSessionWith toMsg model ( session, subCmd ) =
    ( { model | session = session }
    , Cmd.map toMsg subCmd
    )


updateGuestUResult : (subModel -> Status) -> (subMsg -> Msg) -> Model -> UpdateResult subModel subMsg Guest.External -> ( Model, Cmd Msg )
updateGuestUResult toStatus toMsg model uResult =
    List.foldl
        (\commExtMsg ( m, cmds_ ) ->
            case commExtMsg of
                Guest.UpdatedGuest guest ->
                    ( { m | session = Page.Guest guest }
                    , cmds_
                    )
        )
        ( { model | status = toStatus uResult.model }
        , []
        )
        uResult.exts
        |> (\( model_, cmds_ ) ->
                ( model_
                , Cmd.batch
                    (Cmd.map toMsg (Cmd.batch uResult.cmds)
                        :: List.map (Ports.mapAddress toMsg >> Ports.javascriptOutCmd msgToString) uResult.ports
                        ++ List.map (Log.map toMsg >> Log.send msgToString) uResult.logs
                        ++ cmds_
                    )
                )
           )



hideFeedback : Model -> Model
hideFeedback model =
    model


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            model.session

        shared =
            case session of
                Page.Guest guest ->
                    guest.shared

        updateStatus model_ newStatus =
            { model_ | status = newStatus }

        noCmd model_ =
            ( model_, Cmd.none )

        afterRedirect maybeRedirect =
            let
                addRedirect redirect =
                    case model.session of
                        Page.Guest guest ->
                            { model
                                | session =
                                    Guest.addAfterLoginRedirect redirect guest
                                        |> Page.Guest
                                , status = Redirect
                            }
            in
            Maybe.map addRedirect maybeRedirect
                |> Maybe.withDefault model

        withGuest init_ update_ maybeRedirect =
            let
                model_ =
                    afterRedirect maybeRedirect

                fn =
                    init_
                        >> update_ model_
            in
            case session of
                Page.Guest guest ->
                    fn guest

        withFeature feature fn =
            case session of
                Page.Guest guest ->
                    NotFound
                        |> updateStatus model
                        |> noCmd
    in
    case maybeRoute of
        Nothing ->
            NotFound
                |> updateStatus model
                |> noCmd

        Just Route.Root ->
            withGuest
                Home.init
                (updateStatusWith Home GotHomeMsg)
                Nothing

        Just Route.Home ->
            case model.session of
                Page.Guest _ ->
                    withGuest
                        Home.init
                        (updateStatusWith Home GotHomeMsg)
                        Nothing


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg address val =
    Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        ChangedUrl _ ->
            [ "ChangedUrl" ]

        ClickedLink _ ->
            [ "ClickedLink" ]

        GotPageMsg _ ->
            [ "GotPageMsg" ]

        GotHomeMsg _ ->
            [ "GotHomeMsg" ]

                
-- VIEW


view : Model -> Document Msg
view model =
    let
        baseTitle =
            "Covid Aid Tracker"

        fullPageTitle : String -> String
        fullPageTitle subTitle =
            if subTitle == "" then
                baseTitle

            else
                subTitle ++ " | " ++ baseTitle

        viewGuest :
            subModel
            -> Guest.Page
            -> (subMsg -> Msg)
            -> (Guest.Model -> subModel -> { title : String, content : Html subMsg })
            -> Document Msg
        viewGuest subModel page toMsg subView =
            case model.session of
                Page.Guest guest ->
                    let
                        { title, content } =
                            subView guest subModel
                    in
                    Document
                        (fullPageTitle title)
                        [ Html.map toMsg content
                            |> Page.viewGuest GotPageMsg page guest
                        ]

        viewPage :
            Guest.Page
            -> Guest.Page
            -> (subMsg -> Msg)
            -> { title : String, content : Html subMsg }
            -> Document Msg
        viewPage guestPage loggedInPage toMsg { title, content } =
            case model.session of
                Page.Guest guest ->
                    Document (fullPageTitle title)
                        [ Html.map toMsg content
                        |> Page.viewGuest GotPageMsg guestPage guest
                        ]

    in
    case model.status of
        Redirect ->
            viewPage Guest.Other Guest.Other (\_ -> Ignored) { title = "", content = text "" }

        NotFound ->
            viewPage Guest.Other Guest.Other (\_ -> Ignored) { title = "Not Found", content = text "Not Found" }

        Home subModel ->
            viewGuest subModel Guest.Home GotHomeMsg Home.view
