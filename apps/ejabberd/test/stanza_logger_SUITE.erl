-module(stanza_logger_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-define(SL, mod_stanza_logger).

all() ->
    [ stanza_filter ].

init_per_suite(C) ->
    application:start(stringprep),
    C.

stanza_filter(_C) ->
    M = msg(),
%%    F1 = ?SL:make_fun(#{from => F}),
%%    true = F1(F, T, M),
%%    false = F1(T, F, M),
    F2 = ?SL:make_fun(#{user => f()}),
    true = F2(f(), M),
    false = F2(o(), M),
    F3 = ?SL:make_fun(#{user => f(), name => <<"message">>}),
    true = F3(f(), M),
    false = F3(o(), pres()),
    F4 = ?SL:make_fun(#{user => <<"ali">>, name => <<"message">>}),
    true = F4(f(), M),
    false = F4(o(), M),
    ok.

f() -> jid:from_binary(<<"alice@localhost">>).
o() -> jid:from_binary(<<"bob@localhost">>).

msg() ->
    #xmlel{name = <<"message">>}.

pres() ->
    #xmlel{name = <<"presence">>}.

iq() ->
    #xmlel{name = <<"iq">>}.
