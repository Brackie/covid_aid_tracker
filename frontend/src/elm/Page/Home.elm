module Page.Home exposing (..)

import Api
import Html exposing (Html, button, div, img, p, span, text)
import Html.Attributes exposing (class, classList, src, style)
import Html.Events exposing (onClick)
import Iso8601 exposing (fromTime)
import Route exposing (Route(..), replaceUrl)
import Session.Shared as Shared exposing (Shared)
import Time
import UpdateResult as UR
import Visualize.Barchart
import Visualize.Linechart



-- MODEL


type alias Model =
    {
        data : List ()
    }


type Insight = CountyGovtFunding
        | NationalGovtFunding
        | TotalFunding

type alias Config = {
        insight : Insight
    }

type alias SharedModel m =
    { m | shared : Shared }


init : SharedModel m -> ( Model, Cmd Msg )
init shared =
    let
        initModel =
            {
                data = []
            }
    in
    ( initModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp



-- VIEW


view : SharedModel m -> Model -> { title : String, content : Html Msg }
view shared model =
    { title = "Home"
    , content = div [][]
    }


visualize : Model -> Html Msg
visualize model =
    div [][]

-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg ()


update : Msg -> Model -> SharedModel m -> UR.UpdateResult Model Msg extMsg
update msg model { shared } =
    case msg of
        NoOp ->
            UR.init model
