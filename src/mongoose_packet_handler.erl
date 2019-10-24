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

-callback process_packet(Acc :: mongoose_acc:t(), From ::jid:jid(), To ::jid:jid(),
                         El :: exml:element(), Extra :: any()) -> {ok | drop, mongoose_acc:t()}.

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-export([new/1, new/2, process/5]).
-export([filter_local_packet/4]).
%% Getters
-export([module/1, extra/1]).

-spec new(Module :: module()) -> t().
new(Module) ->
    new(Module, undefined).

-spec new(Module :: module(), Extra :: any()) -> t().
new(Module, Extra) when is_atom(Module) ->
    #packet_handler{ module = Module, extra = Extra }.

-spec process(Handler :: t(),
              Acc :: mongoose_acc:t(),
              From ::jid:jid(),
              To ::jid:jid(),
              El :: exml:element()) -> {ok | drop, mongoose_acc:t()}.
process(#packet_handler{ module = Module, extra = Extra }, Acc, From, To, El) ->
    Module:process_packet(Acc, From, To, El, Extra).

module(#packet_handler{ module = Module }) ->
    Module.

extra(#packet_handler{ extra = Extra }) ->
    Extra.

filter_local_packet(OrigFrom, OrigTo, Acc0, OrigPacket) ->
    LDstDomain = OrigTo#jid.lserver,
    case ejabberd_hooks:run_fold(filter_local_packet, LDstDomain,
                                 {OrigFrom, OrigTo, Acc0, OrigPacket}, []) of
        {From, To, Acc, Packet} ->
            Acc1 = mongoose_acc:update_stanza(#{from_jid => From, to_jid => To, element => Packet}, Acc),
            {From, To, Acc1, Packet};
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
