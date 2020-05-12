-module(mongoose_debug).

%% The most simple use case possible is:
%% - add {mongoose_debug, []} to modules in mongooseim.cfg
%% - from erlang shell, run recon_trace:calls([{mongoose_debug, traffic, '_'}], 100, [{scope, local}]).
%% - watch all the traffic coming in and out

-behaviour(gen_mod).

-include("mongoose.hrl").
-include("jlib.hrl").

%% API
-export([start/2, stop/1]).
-export([trace_traffic/2, traffic/4]).

start(Host, _Opts) ->
    ejabberd_hooks:add(hooks(Host)),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)),
    ok.

hooks(Host) ->
    [{c2s_debug, Host, ?MODULE, trace_traffic, 50}].


trace_traffic(Acc, {out, From, El}) ->
    Sfrom = binary_to_list(jid:to_binary(From)),
    Sto = binary_to_list(exml_query:attr(El, <<"to">>, <<>>)),
    St = binary_to_list(exml:to_pretty_iolist(El)),
    Marker = " ---> ",
    traffic(Sfrom, Marker, Sto, St),
    Acc;
trace_traffic(Acc, {in, {From, To, El}}) ->
    Sfrom = binary_to_list(jid:to_binary(From)),
    Sto = binary_to_list(jid:to_binary(To)),
    St = binary_to_list(exml:to_pretty_iolist(El)),
    Marker = " <--- ",
    traffic(Sto, Marker, Sfrom, St),
    Acc.

traffic(_Sender, _Marker, _Recipient, _Stanza) -> ok.

