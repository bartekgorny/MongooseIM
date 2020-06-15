%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc Module providing support for websockets in MongooseIM
%%% @end
%%%===================================================================
-module(mongoose_traffic_channel).

-behaviour(cowboy_websocket).

%% cowboy_http_websocket_handler callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-define(LISTENER, ?MODULE).
-define(MAX_ITEMS, 500).
-define(MAX_ACCOUNTS, 100).

-record(state, {traces = #{}, tracing = false, current = <<>>}).

%%--------------------------------------------------------------------
%% Common callbacks for all cowboy behaviours
%%--------------------------------------------------------------------

init(Req, Opts) ->
    Peer = cowboy_req:peer(Req),
    PeerCert = cowboy_req:cert(Req),
    ?DEBUG("cowboy init: ~p~n", [{Req, Opts}]),
    AllModOpts = [{peer, Peer}, {peercert, PeerCert} | Opts],
    %% upgrade protocol
    {cowboy_websocket, Req, AllModOpts, #{}}.

terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% cowboy_http_websocket_handler callbacks
%%--------------------------------------------------------------------

% Called for every new websocket connection.
websocket_init(Opts) ->
    ?DEBUG("websocket_init: ~p~n", [Opts]),
    gen_server:call(mongoose_traffic, {register, self()}),
    {ok, #state{}}.

% Called when a text message arrives.
websocket_handle({text, Msg}, State) ->
    case handle(jiffy:decode(Msg), State) of
        {ok, State1} ->
            {reply, <<"ok">>, State1};
        {Event, State1} ->
            {reply, reply(Event), State1};
        {Event, Payload, State1} ->
            {reply, reply(Event, Payload), State1}
    end;

websocket_handle({binary, Msg}, State) ->
    ?DEBUG("Received binary: ~p", [Msg]),
    {ok, State};

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(Any, State) ->
    ?DEBUG("Received non-text: ~p", [Any]),
    {ok, State}.

% Other messages from the system are handled here.
websocket_info({message, _Dir, _J, _Stanza}, #state{tracing = false} = State) ->
    {ok, State};
websocket_info({message, Dir, J, Stanza} = Message, State) ->
    {Traces1, IsNew} = record_item(Dir, J, Stanza, State#state.traces),
    case maps:size(Traces1) of
        N when N > ?MAX_ACCOUNTS ->
            reset_and_stop(State);
        _ ->
            store_stanza_and_reply(Traces1, IsNew, Message, State)
    end;
websocket_info(stop, State) ->
    {stop, State};
websocket_info(Info, State) ->
    ?DEBUG("unknown info: ~p", [Info]),
    {ok, State}.

reset_and_stop(State) ->
    State1 = State#state{tracing = false, traces = #{}},
    M = reply(<<"error">>, #{<<"reason">> => <<"too_many_accounts">>}),
    {reply, M, State1}.

store_stanza_and_reply(Traces1, IsNew, {message, Dir, J, Stanza}, State) ->
    State1 = State#state{traces = Traces1},
    Announcement = maybe_announce_new(IsNew, J),
    Msg = maybe_send_current(Dir, J, Stanza, State),
    {reply, Announcement ++ Msg, State1}.

maybe_announce_new(true, J) ->
    [reply(<<"new_trace">>, #{<<"jid">> => J})];
maybe_announce_new(false, _) ->
    [].

maybe_send_current(Dir, J, Stanza, State) ->
    case is_current(J, State) of
        true ->
            M = reply(<<"message">>, #{<<"dir">> => atom_to_binary(Dir, utf8),
                                             <<"stanza">> => Stanza
                  }),
            [M];
        false ->
            []
    end.


handle({Json}, State) ->
    M = maps:from_list(Json),
    handle(maps:get(<<"event">>, M), maps:get(<<"payload">>, M), State).

handle(<<"get_status">>, _, State) ->
    return_status(State);
handle(<<"trace_flag">>, {Payload}, State) ->
     #{<<"value">> := Flag} = maps:from_list(Payload),
     return_status(State#state{tracing = Flag});
handle(<<"get_trace">>, {Payload}, State) ->
    #{<<"jid">> := Jid} = maps:from_list(Payload),
    {<<"get_trace">>,
     #{<<"jid">> => Jid, <<"trace">> => format_trace(maps:get(Jid, State#state.traces, []))},
     State#state{current = Jid}};
handle(<<"clear_all">>, _, State) ->
    {<<"cleared_all">>,
     State#state{traces = #{}, current = <<>>}};
handle(<<"clear">>, #{<<"jid">> := Jid}, State) ->
    {<<"cleared">>,
     #{<<"jid">> => Jid},
     State#state{traces = clear_trace(to_binary(Jid), State#state.traces)}};
handle(<<"heartbeat">>, _, State) ->
    {<<"heartbeat_ok">>,
     <<>>,
     State};
handle(Event, Payload, State) ->
    ?ERROR_MSG("unknown event: ~p carrying ~p", [Event, Payload]),
    {<<"zwrotka">>, <<"bzzzzzzzz">>, State}.


return_status(State) ->
    {<<"status">>,
     #{<<"trace_flag">> => State#state.tracing},
     State}.

reply(Event) ->
    reply(Event, #{}).

reply(Event, Payload) ->
    {text, jiffy:encode(#{<<"event">> => Event, <<"payload">> => Payload})}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

to_binary(#jid{} = Jid) -> jid:to_binary(Jid);
to_binary(Jid) -> Jid.

clear_trace(J, Traces) -> maps:put(J, queue:new(), Traces).

is_current(J, #state{current = J}) -> true;
is_current(_, _)                   -> false.

record_item(Dir, J, Stanza, Traces) ->
    {Tr, IsNew} = case maps:get(J, Traces, undefined) of
             undefined ->
                 {queue:new(), true};
             Q -> {Q, false}
         end,
    Tr1 = queue:in({Dir, Stanza}, Tr),
    Tr2 = case queue:len(Tr1) of
              ?MAX_ITEMS -> queue:out(Tr1);
              _ -> Tr1
          end,
    {maps:put(J, Tr2, Traces), IsNew}.

format_trace(Trace) ->
    lists:map(fun({Dir, Stanza}) -> #{<<"dir">> => atom_to_binary(Dir, utf8), <<"stanza">> => Stanza} end,
              lists:reverse(queue:to_list(Trace))).

