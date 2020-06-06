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

-record(state, {traces = #{}, traceall = true}).
% TODO default false, frontend should remember and set on reconnection

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
websocket_info({message, Dir, J, Stanza}, State) ->
    case is_traced(J, State) of
        true ->
%%            publish_message(J, Dir, Stanza),
            {Traces1, IsNew} = record_item(Dir, J, Stanza, State#state.traces),
            State1 = State#state{traces = Traces1},
            case IsNew of
                true -> {reply, reply(<<"new_trace">>, #{<<"jid">> => J}), State1};
                false -> {ok, State}
            end;
        false ->
            {ok, State}
    end;
websocket_info(stop, State) ->
    {stop, State};
websocket_info(Info, State) ->
    ?DEBUG("unknown info: ~p", [Info]),
    {ok, State}.

handle({Json}, State) ->
    M = maps:from_list(Json),
    handle(maps:get(<<"event">>, M), maps:get(<<"payload">>, M), State).

handle(<<"get_status">>, _, State) ->
    return_status(State);
handle(<<"trace_all">>, {Payload}, State) ->
     #{<<"value">> := Flag} = maps:from_list(Payload),
     return_status(State#state{traceall = Flag});
handle(<<"get_trace">>, #{<<"jid">> := Jid}, State) ->
    {<<"get_trace">>,
     #{<<"jid">> => Jid, <<"trace">> => maps:get(Jid, State#state.traces, [])},
     State};
handle(<<"clear_all">>, _, State) ->
    {<<"cleared_all">>,
     State#state{traces = clear_all(State#state.traces)}};
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
     #{<<"trace_all">> => State#state.traceall},
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

clear_all(Traces) -> lists:foldl(fun(J, T) -> maps:put(J, queue:new(), T) end, #{}, maps:keys(Traces)).

is_traced(_, #state{traceall = true}) -> true;
is_traced(J, #state{traces = Traces}) -> maps:is_key(J, Traces).

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

publish_message(_J, _Dir, _Stanza) ->
    % somehow send to an appropriate channel
    ok.

announce_new_trace(_J) ->
    % so that you know there is something to watch (if you are tracing all)
    ok.



%%add_trace(J, Traces) ->
%%    case maps:get(J, Traces, none) of
%%        none -> maps:put(J, queue:new(), Traces);
%%        _ -> Traces
%%    end.

%%remove_trace(J, Traces) -> maps:remove(J, Traces).

%%%===================================================================
%%% API
%%%===================================================================
%%handle_call({trace, Jid}, _From, State) ->
%%    {reply, ok, State#state{traces =  add_trace(to_binary(Jid), State#state.traces)}};
%%handle_call({untrace, Jid}, _From, State) ->
%%    {reply, ok, State#state{traces =  remove_trace(to_binary(Jid), State#state.traces)}};
%%handle_call({clear, Jid}, _From, State) ->
%%    {reply, ok, State#state{traces =  clear_trace(to_binary(Jid), State#state.traces)}};
%%handle_call(clear_all, _From, State) ->
%%    {reply, ok, State#state{traces =  clear_all(State#state.traces)}};
%%handle_call(untrace_all, _From, State) ->
%%    {reply, ok, State#state{traces = #{}}};
%%handle_call({traceall, Flag}, _From, State) ->
%%    {reply, ok, State#state{traceall = Flag}};
%%handle_call(_Request, _From, State) ->
%%    {reply, ok, State}.
%%
%%handle_cast({message, Dir, J, Stanza}, State) ->
%%    case is_traced(J, State) of
%%        true ->
%%            publish_message(J, Dir, Stanza),
%%            {noreply, State#state{traces = record_item(Dir, J, Stanza, State#state.traces)}};
%%        false ->
%%            {noreply, State}
%%    end;
%%handle_cast(_Request, State) ->
%%    {noreply, State}.
%%
%%handle_info(_Info, State) ->
%%    {noreply, State}.
%%
%%terminate(_Reason, _State) ->
%%    ok.
%%
%%code_change(_OldVsn, State, _Extra) ->
%%    {ok, State}.
