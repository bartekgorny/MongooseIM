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

type alias Stanza = { dir : String, stanza : String}
type alias Model = { tracing : Bool,
                     traced_jids : List String,
                     current_jid : String,
                     stanzas : List Stanza}

type Msg = SetStatus Bool
--           | Test
--           | GetStatus
           | ClearAll
           | SelectJid String
           | RecEvent Encode.Value

init : () -> (Model, Cmd Msg)
init _ = ({ tracing = False,
            traced_jids = [],
            current_jid = "",
            stanzas = []},
          outPort(simpleEvent "get_status")) -- server default may change

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
--        Test -> (model, outPort ((Encode.string "test", Encode.string "bzzz")))
--        GetStatus -> (model, outPort(simpleEvent "get_status"))
        ClearAll -> (model, outPort(simpleEvent "clear_all"))
        SetStatus st ->
            case st of
                True -> (model, outPort(outEvent "trace_flag" [("value", Encode.bool True)]))
                False -> (model, outPort(outEvent "trace_flag" [("value", Encode.bool False)]))
        SelectJid jid -> ({model | current_jid = jid}, outPort(outEvent "get_trace" [("jid", Encode.string jid)]))
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
        "cleared_all" -> ({model | traced_jids = [], stanzas = []}, Cmd.none)
        "get_trace" -> handleGetTrace v model
        "message" -> handleMessage v model
        _ -> (model, Cmd.none)

handleStatus v model =
    case v |> decodeField "trace_flag" Decode.bool of
        Ok trace_flag ->
            ({model | tracing = trace_flag}, Cmd.none)
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)

handleNewTrace v model =
    case v |> decodeField "jid" Decode.string of
        Ok jid ->
            ({model | traced_jids = jid :: model.traced_jids}, Cmd.none)
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)

handleGetTrace v model =
    case v |> decodeField "trace" (Decode.list decodeStanza) of
        Ok stanzas ->
            ({model | stanzas = stanzas}, Cmd.none)
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)

handleMessage v model =
    case v |> Decode.decodeValue (Decode.field "payload" decodeStanza)  of
        Ok stanza ->
            ({model | stanzas = stanza :: model.stanzas}, Cmd.none)
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)


decodeStanza = Decode.map2 Stanza (Decode.field "dir" Decode.string) (Decode.field "stanza" Decode.string)

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
            showEnabled model.tracing,
            showEnableButton model.tracing,
--            button [onClick GetStatus] [ text "check"],
            button [onClick ClearAll] [ text "clear all"],
            div [][text model.current_jid],
--            button [onClick Test] [ text "test"],
            viewJids model.traced_jids,
            viewStanzas model.stanzas
           ]


viewJids traced_jids =
    div [] [
        div [] [text "Now tracing:"],
        div [] (List.map showJid (List.reverse traced_jids))
    ]

showJid jid =
    div [] [a [onClick (SelectJid jid)] [text jid]]

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

viewStanzas stanzas =
    div [] [
        div [] [text "Stanzas"],
        div [] (List.map showStanza (List.reverse stanzas))
    ]


showStanza stanza =
    pre [] [text stanza.stanza]
