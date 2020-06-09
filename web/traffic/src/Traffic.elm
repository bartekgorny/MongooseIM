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

type alias Stanza = { dir : String, stanza : String}
type alias Model = { tracing : Bool,
                     traced_jids : List String,
                     current_jid : String,
                     stanzas : List Stanza}

type Msg = SetStatus Bool
           | ClearAll
           | SelectJid String
           | RecEvent Encode.Value


-- UPDATE

init : () -> (Model, Cmd Msg)
init _ = ({ tracing = False,
            traced_jids = [],
            current_jid = "",
            stanzas = []},
          outPort(simpleEvent "get_status")) -- server default may change

update : Msg -> Model -> (Model, Cmd Msg)
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

handleEvent : String -> Decode.Value -> Model -> (Model, Cmd Msg)
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


handleStatus : Decode.Value -> Model -> (Model, Cmd Msg)
handleStatus v model =
    v |> decodeField "trace_flag" Decode.bool
      |> handleError handleStatusOk model

handleStatusOk trace_flag model = ({model | tracing = trace_flag}, Cmd.none)
--    case v |> decodeField "trace_flag" Decode.bool of
--        Ok trace_flag ->
--            ({model | tracing = trace_flag}, Cmd.none)
--        Err error ->
--            let x = Debug.log "error" error in
--            (model, Cmd.none)

handleNewTrace : Decode.Value -> Model -> (Model, Cmd Msg)
handleNewTrace v model =
    case v |> decodeField "jid" Decode.string of
        Ok jid ->
            ({model | traced_jids = jid :: model.traced_jids}, Cmd.none)
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)

handleGetTrace : Decode.Value -> Model -> (Model, Cmd Msg)
handleGetTrace v model =
    case v |> decodeField "trace" (Decode.list decodeStanza) of
        Ok stanzas ->
            ({model | stanzas = stanzas}, Cmd.none)
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)

handleMessage : Decode.Value -> Model -> (Model, Cmd Msg)
handleMessage v model =
    case v |> Decode.decodeValue (Decode.field "payload" decodeStanza)  of
        Ok stanza ->
            ({model | stanzas = stanza :: model.stanzas}, Cmd.none)
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)

handleError : (a -> Model -> (Model, Cmd Msg)) -> Model -> Result Decode.Error a -> (Model, Cmd Msg)
handleError func model result =
    case result of
        Ok res ->
            func res model
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)

decodeStanza = Decode.map2 Stanza (Decode.field "dir" Decode.string) (Decode.field "stanza" Decode.string)

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

