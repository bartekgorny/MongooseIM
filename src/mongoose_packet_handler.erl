%%%----------------------------------------------------------------------
%%% File    : mongoose_packet_handler.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Packet handler behaviour
%%% Created : 24 Jan 2017
%%%----------------------------------------------------------------------

-module(mongoose_packet_handler).
-author('piotr.nosek@erlang-solutions.com').

-include("jlib.hrl").

%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-record(packet_handler, { module, extra }).

-type t() :: #packet_handler{
                module :: module(),
                extra :: any()
               }.

-export_type([t/0]).

%%----------------------------------------------------------------------
%% Callback declarations
%%----------------------------------------------------------------------

-callback process_packet(Host :: jid:lserver(), Acc :: mongoose_acc:t(), From ::jid:jid(), To ::jid:jid(),
                         El :: exml:element(), Extra :: any()) -> {ok | drop, mongoose_acc:t()}.

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-export([new/1, new/2, process/6]).
-export([filter_local_packet/5]).
%% Getters
-export([module/1, extra/1]).

-spec new(Module :: module()) -> t().
new(Module) ->
    new(Module, undefined).

-spec new(Module :: module(), Extra :: any()) -> t().
new(Module, Extra) when is_atom(Module) ->
    #packet_handler{ module = Module, extra = Extra }.

-spec process(Host :: jid:lserver(), Handler :: t(),
              Acc :: mongoose_acc:t(),
              From ::jid:jid(),
              To ::jid:jid(),
              El :: exml:element()) -> {ok | drop, mongoose_acc:t()}.
process(Host, #packet_handler{ module = Module, extra = Extra }, Acc, From, To, El) ->
    Module:process_packet(Host, Acc, From, To, El, Extra).

module(#packet_handler{ module = Module }) ->
    Module.

extra(#packet_handler{ extra = Extra }) ->
    Extra.

filter_local_packet(Host, OrigFrom, OrigTo, Acc0, OrigPacket) ->
    case mongoose_acc:get(hook_result, filter_local_packet, undefined, Acc0) of
        undefined ->
            {Res, Acc1} = run_filter_local_packet(Host, OrigFrom, OrigTo, Acc0, OrigPacket),
            Acc2 = mongoose_acc:set(hook_result, filter_local_packet, Res, Acc1),
            {Res, Acc2};
        drop ->
            {drop, Acc0};
        ok ->
            {ok, Acc0}
    end.

run_filter_local_packet(Host, OrigFrom, OrigTo, Acc0, OrigPacket) ->
    case ejabberd_hooks:run_fold(filter_local_packet, Host,
                                 {OrigFrom, OrigTo, Acc0, OrigPacket}, []) of
        {From, To, Acc, Packet} ->
            Acc1 = mongoose_acc:update_stanza(#{from_jid => From, to_jid => To, element => Packet}, Acc),
            {ok, Acc1};
        drop ->
            ejabberd_hooks:run(xmpp_stanza_dropped,
                               OrigFrom#jid.lserver,
                               [OrigFrom, OrigTo, OrigPacket]),
            {drop, Acc0};
        {drop, Acc} ->
            ejabberd_hooks:run(xmpp_stanza_dropped,
                               OrigFrom#jid.lserver,
                               [OrigFrom, OrigTo, OrigPacket]),
            {drop, Acc}
    end.
