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

type alias Jid = String
type alias Stanza = { dir : String, stanza : String}
type alias Model = { tracing : Bool,
                     traced_jids : List Jid,
                     current_jid : Jid,
                     stanzas : List Stanza}

type alias DecodeResult a = Result Decode.Error a
type alias UpdateResult = (Model, Cmd Msg)

type Msg = SetStatus Bool
           | ClearAll
           | SelectJid Jid
           | RecEvent Encode.Value


-- UPDATE

init : () -> (Model, Cmd Msg)
init _ = ({ tracing = False,
            traced_jids = [],
            current_jid = "",
            stanzas = []},
          outPort(simpleEvent "get_status")) -- server default may change

update : Msg -> Model -> UpdateResult
update msg model =
    case msg of 
        ClearAll -> (model, outPort(simpleEvent "clear_all"))
        SetStatus st ->
                (model, setTraceEvent st)
        SelectJid jid -> ({model | current_jid = jid}, outPort(outEvent "get_trace" [("jid", Encode.string jid)]))
        RecEvent v ->
            case Decode.decodeValue (Decode.field "event" Decode.string) v of
                Ok eventName ->
                    let z = Debug.log "v" eventName in
                    handleEvent eventName v model
                Err error ->
                    let x = Debug.log "error" error in
                    (model, Cmd.none)


-- INCOMING

handleEvent : String -> Decode.Value -> Model -> UpdateResult
handleEvent ename v model =
    case ename of
        "status" -> handleStatus v model
        "new_trace" -> handleNewTrace v model
        "cleared_all" -> (clearAll model, Cmd.none)
        "get_trace" -> handleGetTrace v model
        "message" -> handleMessage v model
        "reinitialise" -> (clearAll model, setTraceEvent model.tracing) -- server was probably restarted, we set our status
        _ -> (model, Cmd.none)

clearAll : Model -> Model
clearAll model = {model | traced_jids = [], stanzas = [], current_jid = ""}

setTraceEvent : Bool -> Cmd Msg
setTraceEvent st = outPort(outEvent "trace_flag" [("value", Encode.bool st)])


handleStatus : Decode.Value -> Model -> UpdateResult
handleStatus v model =
    (v, model)
    |> handleDecodedValue (decodeField "trace_flag" Decode.bool)
                          handleStatusOk


handleDecodedValue : (Decode.Value -> DecodeResult a) -- decoder
                      -> (Model -> a -> UpdateResult) -- handler if ok
                      -> (Decode.Value, Model)
                      -> UpdateResult
handleDecodedValue decoder handler (v, model) =
    case decoder v of
        Ok res -> handler model res
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)

handleStatusOk : Model -> Bool -> UpdateResult
handleStatusOk model trace_flag = ({model | tracing = trace_flag}, Cmd.none)

handleNewTrace : Decode.Value -> Model -> UpdateResult
handleNewTrace v model =
    (v, model)
    |> handleDecodedValue (decodeField "jid" Decode.string )
                          handleNewTraceOk

handleNewTraceOk : Model -> Jid -> UpdateResult
handleNewTraceOk model jid =
    ({model | traced_jids = jid :: model.traced_jids}, Cmd.none)

handleGetTrace : Decode.Value -> Model -> UpdateResult
handleGetTrace v model =
    (v, model)
    |> handleDecodedValue (decodeField "trace" (Decode.list decodeStanza))
                          handleDecodedValueOk

handleDecodedValueOk : Model -> List Stanza -> UpdateResult
handleDecodedValueOk model stanzas =
    ({model | stanzas = stanzas}, Cmd.none)

handleMessage : Decode.Value -> Model -> UpdateResult
handleMessage v model =
    (v, model)
    |> handleDecodedValue (Decode.decodeValue (Decode.field "payload" decodeStanza))
                          handleMessageOk

handleMessageOk : Model -> Stanza -> UpdateResult
handleMessageOk model stanza =
    ({model | stanzas = stanza :: model.stanzas}, Cmd.none)

decodeStanza = Decode.map2 Stanza (Decode.field "dir" Decode.string) (Decode.field "stanza" Decode.string)

decodeField : String -> Decode.Decoder a -> Decode.Value -> DecodeResult a
decodeField fieldname decoder v =
    Decode.decodeValue  (Decode.field "payload" (Decode.field fieldname decoder)) v


-- COMMUNICATION TOOLS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [incPort RecEvent]

port outPort : (Encode.Value, Encode.Value) -> Cmd msg
port incPort : (Encode.Value -> msg) -> Sub msg

emptyPayload = Encode.object []

simpleEvent : String -> (Encode.Value, Encode.Value)
simpleEvent evt = (Encode.string evt, emptyPayload)


outEvent : String -> List (String, Encode.Value) -> (Encode.Value, Encode.Value)
outEvent evtname payload =
    (Encode.string evtname,
     Encode.object payload)


-- VIEW

view : Model -> Html Msg
view model =
    div [class "all"][
        div [class "top"][
            div [class "header"][text "MongooseIM traffic tracer"],
            showEnableButton model.tracing,
            button [class "clearButton", onClick ClearAll] [ text "clear all"]
        ],
        div [class "main"][
            div [class "left"][
                viewJids model.traced_jids
            ],
            div [class "right"][
                div [class "current"][text model.current_jid],
                viewStanzas model.stanzas
            ]
        ]
    ]


viewJids traced_jids =
    div [class "tracing"] [
        div [class "label"] [text "Active accounts:"],
        div [class "jids"] (List.map showJid (List.reverse traced_jids))
    ]

showJid jid =
    div [class "jid"] [a [onClick (SelectJid jid)] [text jid]]

enableClass is_enabled =
    case is_enabled of
        True -> "true"
        False -> "false"

showEnableButton is_enabled =
    div [class "enabled"][
        button [class (enableClass is_enabled),
                onClick (SetStatus (not is_enabled))]
                [ text "Tracing"]
    ]

viewStanzas stanzas =
    div [class "stanzas"] [
        div [class "label"] [text "Stanzas"],
        div [class "stanzalist"] (List.map showStanza (List.reverse stanzas))
    ]


showStanza stanza =
    div [class ("stanza " ++ stanza.dir)]
        (List.map showStanzaPart (String.split "\n" stanza.stanza))

showStanzaPart p =
    div [class "part"][text p]

