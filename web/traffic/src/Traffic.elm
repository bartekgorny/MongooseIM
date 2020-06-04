port module Traffic exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import Json.Encode as Encode
import Json.Decode as Decode
import Dict

main = Browser.element {init = init, view = view, update = update, subscriptions = subscriptions}

-- TYPES

type alias Model = {}

type Msg = Test

init : () -> (Model, Cmd Msg)
init _ = ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Test -> (model, testPort (Encode.string "bzzz"))

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch []

port testPort : Encode.Value -> Cmd msg

view : Model -> Html Msg
view model =
    div [] [div [][text "hehehe"],
            button [onClick Test] [ text "test"]
           ]
