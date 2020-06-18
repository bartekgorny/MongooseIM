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

-record(state, {traces = #{},
                tracing = false,
                current = <<>>,
                mappings = #{},
                start_times = #{}}).

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
websocket_info({message, Dir, {Pid, Jid}, Stanza} = Message, State) ->
    Spid = pid_to_binary(Pid),
    Now = now_seconds(),
    {Traces1, Mappings, IsNew} = record_item(Now,
                                             Dir,
                                             {Spid, Jid},
                                             Stanza,
                                             State#state.traces,
                                             State#state.mappings),
    State1 = State#state{mappings = Mappings},
    State2 = maybe_store_start_time(Spid, Now, State1),
    case maps:size(Traces1) of
        N when N > ?MAX_ACCOUNTS ->
            reset_and_stop(State1);
        _ ->
            store_stanza_and_reply(Now, Traces1, IsNew, Message, State2)
    end;
websocket_info(stop, State) ->
    {stop, State};
websocket_info(Info, State) ->
    ?DEBUG("unknown info: ~p", [Info]),
    {ok, State}.

reset_and_stop(State) ->
    State1 = State#state{tracing = false},
    M = reply(<<"error">>, #{<<"reason">> => <<"too_many_accounts">>}),
    {reply, M, State1}.

store_stanza_and_reply(Now, Traces1, IsNew, {message, Dir, {Pid, Jid}, Stanza}, State) ->
    Spid = pid_to_binary(Pid),
    State1 = State#state{traces = Traces1},
    Announcement = maybe_announce_new(IsNew, Spid, Jid),
    Msg = maybe_send_current(Now, Dir, Spid, Stanza, State),
    {reply, Announcement ++ Msg, State1}.

maybe_announce_new(true, Spid, Jid) ->
    {BareJid, FullJid} = format_jid(Jid),
    [reply(<<"new_trace">>,
           #{<<"pid">> => Spid,
             <<"bare_jid">> => BareJid,
             <<"full_jid">> => FullJid})];
maybe_announce_new(false, _, _) ->
    [].

maybe_send_current(Now, Dir, Spid, Stanza, State) ->
    case is_current(Spid, State) of
        true ->
            Tm = Now - maps:get(Spid, State#state.start_times),
            M = reply(<<"message">>, #{<<"dir">> => atom_to_binary(Dir, utf8),
                                       <<"time">> => Tm,
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
    #{<<"pid">> := Pid} = maps:from_list(Payload),
    {<<"get_trace">>,
     #{<<"pid">> => Pid, <<"trace">> => format_trace(maps:get(Pid, State#state.traces, []),
                                                     maps:get(Pid, State#state.start_times))},
     State#state{current = Pid}};
handle(<<"clear_all">>, _, State) ->
    {<<"cleared_all">>,
     State#state{traces = #{}, current = <<>>}};
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

is_current(J, #state{current = J}) -> true;
is_current(_, _)                   -> false.

record_item(Time, Dir, {Spid, Jid}, Stanza, Traces, Mappings) ->
    Tr = case maps:get(Spid, Traces, undefined) of
             undefined ->
                 queue:new();
             Q -> Q
         end,
    Fjid = format_jid(Jid),
    IsNew = is_new_mapping(maps:get(Spid, Mappings, undefined), Fjid),
    Mappings1 = case IsNew of
                    true ->
                        maps:put(Spid, Fjid, Mappings);
                    false ->
                        Mappings
                end,
    Tr1 = queue:in({Time, Dir, Stanza}, Tr),
    Tr2 = case queue:len(Tr1) of
              ?MAX_ITEMS -> queue:out(Tr1);
              _ -> Tr1
          end,
    {maps:put(Spid, Tr2, Traces), Mappings1, IsNew}.

format_trace(Trace, StartTime) ->
    lists:map(fun({Time, Dir, Stanza}) ->
                  #{<<"dir">> => atom_to_binary(Dir, utf8),
                    <<"time">> => Time - StartTime,
                    <<"stanza">> => Stanza}
              end,
              lists:reverse(queue:to_list(Trace))).

pid_to_binary(Pid) when is_pid(Pid) ->
    [Spid] = io_lib:format("~p", [Pid]),
    list_to_binary(Spid).

format_jid(undefined) ->
    {<<>>, <<>>};
format_jid(#jid{resource = <<>>} = Jid) ->
    {jid:to_binary(jid:to_lower(Jid)), <<>>};
format_jid(#jid{} = Jid) ->
    {<<>>, jid:to_binary(jid:to_lower(Jid))}.

is_new_mapping(undefined,    {_, _})       -> true; % showed up for the very first time
is_new_mapping({_, _},       {<<>>, <<>>}) -> false; % nothing new
is_new_mapping({<<>>, <<>>}, {_, _})       -> true; % nothing old, something new
is_new_mapping({<<>>, _},    {_, _})       -> false; % we already have full jid
is_new_mapping({_, <<>>},    {<<>>, _})    -> true; % we have bare, received full
is_new_mapping(_,            {_, _})       -> false.

now_seconds() ->
    {Msec, Sec, Mili} = os:timestamp(),
    Msec * 1000000 + Sec + (Mili / 1000000).

maybe_store_start_time(Spid, Time, #state{start_times = StartTimes} = State) ->
    case maps:get(Spid, StartTimes, undefined) of
        undefined ->
            State#state{start_times = maps:put(Spid, Time, StartTimes)};
        _ ->
            State
    end.
