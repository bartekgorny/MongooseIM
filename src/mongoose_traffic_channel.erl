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

-define(LISTENER, ?MODULE).

-record(ws_state, {}).

%%--------------------------------------------------------------------
%% Common callbacks for all cowboy behaviours
%%--------------------------------------------------------------------

init(Req, Opts) ->
    Peer = cowboy_req:peer(Req),
    PeerCert = cowboy_req:cert(Req),
    ?DEBUG("cowboy init: ~p~n", [{Req, Opts}]),
%%    Timeout = gen_mod:get_opt(timeout, Opts, 60000),

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
%%    send_ping_request(10000),
    ?DEBUG("websocket_init: ~p~n", [Opts]),
    State = #ws_state{ },
    {ok, State}.

% Called when a text message arrives.
websocket_handle({text, Msg}, State) ->
    case handle(jiffy:decode(Msg)) of
        ok -> {ok, State};
        {Reply, Payload} ->
            {reply, {text, jiffy:encode(#{<<"event">> => Reply, <<"payload">> => Payload})}, State}
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
websocket_info({do_ping, PingRate}, State) ->
    %% send ping frame to the client
    send_ping_request(PingRate),
    {reply, ping, State};
websocket_info(stop, State) ->
    {stop, State};
websocket_info(Info, State) ->
    ?DEBUG("unknown info: ~p", [Info]),
    {ok, State}.

send_ping_request(PingRate) ->
    Dest = self(),
    ?DEBUG("Sending websocket ping request to ~p", [Dest]),
    erlang:send_after(PingRate, Dest, {do_ping, PingRate}).

handle({Json}) ->
    M = maps:from_list(Json),
    handle(maps:get(<<"event">>, M), maps:get(<<"payload">>, M)).

handle(Event, Payload) ->
    ?ERROR_MSG("unknown event: ~p carrying ~p", [Event, Payload]),
    {<<"zwrotka">>, <<"bzzzzzzzz">>}.
