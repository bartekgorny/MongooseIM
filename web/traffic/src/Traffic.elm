port module Traffic exposing (main, handleEvent)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import Json.Encode as Encode
import Json.Decode as Decode
import Dict

main = Browser.element {init = init, view = view, update = update, subscriptions = subscriptions}

-- TYPES

type alias Model = { tracing_all : Bool, traced_jids : List String}

type Msg = Test
           | GetStatus
           | SetStatus Bool
           | ClearAll
           | RecEvent Encode.Value

init : () -> (Model, Cmd Msg)
init _ = ({ tracing_all = False, traced_jids = ["zzz", "eee", "aaa"]},
          outPort(simpleEvent "get_status")) -- server default may change

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Test -> (model, outPort ((Encode.string "test", Encode.string "bzzz")))
        GetStatus -> (model, outPort(simpleEvent "get_status"))
        ClearAll -> (model, outPort(simpleEvent "clear_all"))
        SetStatus st ->
            case st of
                True -> (model, outPort(outEvent "trace_all" [("value", Encode.bool True)]))
                False -> (model, outPort(outEvent "trace_all" [("value", Encode.bool False)]))
        RecEvent v ->
            case Decode.decodeValue (Decode.field "event" Decode.string) v of
                Ok eventName ->
                    let z = Debug.log "v" eventName in
                    handleEvent eventName v model
                Err error ->
                    let x = Debug.log "error" error in
                    (model, Cmd.none)


handleEvent : String -> Decode.Value -> Model -> (Model, Cmd Msg)
handleEvent ename v model =
    case ename of
        "status" -> handleStatus v model
        "new_trace" -> handleNewTrace v model
        "cleared_all" -> ({model | traced_jids = []}, Cmd.none)
        _ -> (model, Cmd.none)

handleStatus v model =
    case decodeField "trace_all" Decode.bool v of
        Ok traceall ->
            ({model | tracing_all = traceall}, Cmd.none)
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)

handleNewTrace v model =
    case decodeField "jid" Decode.string v of
        Ok jid ->
            ({model | traced_jids = jid :: model.traced_jids}, Cmd.none)
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)


decodeField fieldname decoder v =
    Decode.decodeValue  (Decode.field "payload" (Decode.field fieldname decoder)) v

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [incPort RecEvent]

port outPort : (Encode.Value, Encode.Value) -> Cmd msg
port incPort : (Encode.Value -> msg) -> Sub msg

emptyPayload = Encode.object []
simpleEvent evt = (Encode.string evt, emptyPayload)

outEvent evtname payload =
    (Encode.string evtname,
     Encode.object payload)

view : Model -> Html Msg
view model =
    div [] [
            showEnabled model.tracing_all,
            showEnableButton model.tracing_all,
            button [onClick GetStatus] [ text "check"],
            button [onClick ClearAll] [ text "clear all"],
            div [][text "hehehe"],
            button [onClick Test] [ text "test"],
            viewJids model.traced_jids
           ]


viewJids traced_jids =
    div [] [
        div [] [text "Now tracing"],
        div [] (List.map showJid (List.reverse traced_jids))
    ]

showJid jid =
    div [] [text jid]

showEnabled is_enabled =
    div [] [text (enableLabel is_enabled)]

enableLabel is_enabled =
    case is_enabled of
        True -> "Enabled"
        False -> "Disabled"

showEnableButton is_enabled =
    button [onClick (SetStatus (not is_enabled))][ text (enableButton is_enabled)]

enableButton is_enabled =
    case is_enabled of
        True -> "Disable"
        False -> "Enable"

