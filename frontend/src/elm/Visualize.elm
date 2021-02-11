module Visualize exposing (..)

import Axis
import Time
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))

w : Float
w =
    900

h:  Float
h =
    450

padding : Float
padding =
    30

xScale : List ( a, Float ) -> BandScale a
xScale model =
    List.map Tuple.first model
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2}
           (0, w - 2 * padding )

yScale : ContinuousScale Float
yScale =
    Scale.linear (h - 2 * padding, 0) (0, 5)


xAxis : List (a, Float) -> (a -> String)->  Svg msg
xAxis model toStr =
    Axis.bottom [] (Scale.toRenderable toStr (xScale model))


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale

-- bar chart

column :  BandScale a -> (a,  Float) -> (a -> String )-> Svg msg
column scale (xval, yval) toStr =
    g [ class [ "column"]]
        [ rect
              [ x <| Scale.convert scale xval
              , y <| Scale.convert yScale yval
              , width <| Scale.bandwidth scale
              , height <| h - Scale.convert yScale yval - 2 * padding
              ]
              []
        , text_
            [ x <| Scale.convert (Scale.toRenderable toStr scale) xval
             , y <| Scale.convert yScale yval - 5
             , textAnchor AnchorMiddle
            ]
            [ text <| String.fromFloat yval ]
        ]
