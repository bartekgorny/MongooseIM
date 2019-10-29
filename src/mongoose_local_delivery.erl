%%%-------------------------------------------------------------------
%%% @doc
%%% Completes delivery to local recipient or a component; called by
%%% main routing chain if it finds a handler to direct the message to.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_local_delivery).
-author('bartlomiej.gorny@erlang-solutions.com').

-include("mongoose.hrl").
-include("jlib.hrl").

%% API
-export([do_route/6]).


do_route(OrigFrom, OrigTo, OrigAcc, OrigPacket, LDstDomain, Handler) ->
    % strip acc from all sender-related values, from now on we are interested in the recipient
    Acc0 = mongoose_acc:strip(#{lserver => OrigTo#jid.lserver,
                                from_jid => OrigFrom,
                                to_jid => OrigTo,
                                element => OrigPacket},
                              OrigAcc),
    %% BTW, return value from this function is ignored
    mongoose_packet_handler:process(LDstDomain, Handler, Acc0, OrigFrom, OrigTo, OrigPacket).
