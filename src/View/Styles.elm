module View.Styles exposing (..)

import Html exposing (..)
import State.Types exposing (..)
import Html.Attributes exposing (style)
import Style exposing (..)
import Color exposing (..)
import Color.Convert exposing (colorToHex)


type alias Styles =
    List Style


colorAttribute : Color -> Attribute Msg
colorAttribute color =
    style [ ( "color", colorToHex color ) ]


colorFailed : Bool -> Attribute Msg
colorFailed b =
    if b then
        colorAttribute red
    else
        colorAttribute black


colorCurrent : Difficulty -> Difficulty -> Attribute Msg
colorCurrent d diff =
    if d == diff then
        colorAttribute darkOrange
    else
        style []


pageStyles : Styles
pageStyles =
    [ marginTop "80px", marginLeft "80px", marginRight "160px", ( "font", "Tahoma, Geneva, sans-serif" ) ]


{-| First argument is whether the text should be enlarged
-}
tibetanText : Bool -> Attribute Msg
tibetanText b =
    if not b then
        style [ ( "font-size", "140%" ) ]
    else
        style [ ( "font-size", "280%" ) ]


largeText : Bool -> Attribute Msg
largeText b =
    if b then
        style [ ( "font-size", "200%" ) ]
    else
        style []
