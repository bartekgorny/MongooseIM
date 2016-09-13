%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2016 20:21
%%%-------------------------------------------------------------------
-module(mod_stanza_logger).
-author("bartekgorny").

-include("ejabberd.hrl").
-include("jlib.hrl").

%% API
-export([make_fun/1]). % for testing

-export([start/1, stop/0, test/0, tstart/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-export([trace/2, untrace/1, gettracers/0]).

test() ->
    M = #xmlel{name = <<"message">>},
    {F, T} = {jid:from_binary(<<"alice@localhost">>),
        jid:from_binary(<<"bob@localhost">>)},
    ejabberd_hooks:run(user_send_packet, <<"localhost">>, [F, T, M]).

tstart() ->
    start(<<"localhost">>),
    trace(a, #{user => <<"ali">>}),
    ok.

-record(state, {host, tracers = #{}}).

start(Host) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Host], []).

stop() ->
    gen_server:call(?MODULE, stop).

trace(Name, Def) when is_atom(Name), is_map(Def) ->
    gen_server:call(?MODULE, {trace, Name, Def}).

untrace(Name) when is_atom(Name) ->
    gen_server:call(?MODULE, {untrace, Name}).

gettracers() ->
    gen_server:call(?MODULE, get).

init([Host]) ->
    {ok, #state{host = Host}}.

handle_call(get, _From, State) ->
    {reply, {ok, maps:keys(State#state.tracers)}, State};
handle_call({trace, Name, Def}, _From, #state{tracers = T, host = Host} = State) ->
    % un-trace first
    T1 = untrace(Name, Host, T),
    Fout = make_logfun(Name, Def, out),
    Fin = make_logfun(Name, Def, in),
    ejabberd_hooks:add(user_send_packet, Host, Fout, 1),
    ejabberd_hooks:add(xmpp_send_element, Host, Fin, 1),
    T2 = maps:put(Name, {Def, Fout, Fin}, T1),
    {reply, ok, State#state{tracers = T2}};
handle_call({untrace, Name}, _From, #state{tracers = T, host = Host} = State) ->
    T1 = untrace(Name, Host, T),
    {reply, ok, State#state{tracers = T1}};
handle_call(stop, _From, State) ->
    {stop, stopped, State};
handle_call(_Event, _From, State) ->
    {reply, ok, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{tracers = T, host = Host} = State) ->
    untrace_all(Host, T),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


make_logfun(Name, Def, out) ->
    Ff = make_fun(Def),
    fun(F, _T, M) ->
        case Ff(F, M) of
            true ->
                Mm = format_message(Name, M),
                % we want output in another log file
                lager:log(critical, self(), "|~p|~n|~p|==> From: ~p~n|~p| ~s",
                    [Name, Name, to_list(F), Name, Mm]);
%%                lager:log(critical, self(), "|~p|~n|~p|==> From: ~p~n|~p|==> To: ~p~n|~p|==> Stanza: ~s",
%%                    [Name, Name, to_list(F), Name, to_list(T), Name, Mm]);
            false ->
                ok
        end
    end;
make_logfun(Name, Def, in) ->
    Ff = make_fun(Def),
    fun(_, T, M) ->
        case Ff(T, M) of
            true ->
                Mm = format_message(Name, M),
                % we want output in another log file
                lager:log(critical, self(), "|~p|~n|~p|<-- To: ~p~n|~p| ~s",
                    [Name, Name, to_list(T), Name, Mm]);
%%                lager:log(critical, self(), "|~p|~n|~p|<-- From: ~p~n|~p|<-- To: ~p~n|~p|<-- Stanza: ~s",
%%                    [Name, Name, to_list(F), Name, to_list(T), Name, Mm]);
            false ->
                ok
        end
    end.


make_fun(Def) ->
    Filters = lists:map(fun make_filter/1, maps:to_list(Def)),
    fun(U, M) ->
        apply_filterlist(U, M, Filters)
    end.

apply_filterlist(_, _, []) ->
    true;
apply_filterlist(U, M, [Fltr|Tail]) ->
    case Fltr(U, M) of
        true ->
            apply_filterlist(U, M, Tail);
        false ->
            false
    end.

make_filter({_, undefined}) ->
    fun(_, _) -> true end;
%%make_filter({from, J}) ->
%%    fun(F, _, _) -> match_jids(F, J) end;
%%make_filter({to, J}) ->
%%    fun(_, T, _) -> match_jids(T, J) end;
make_filter({name, N}) ->
    fun(_, M) ->
        case M of
            #xmlel{name = N} -> true;
            _ -> false
        end
    end;
make_filter({user, J}) ->
    fun(Jid, _) ->
        match_jids(to_jid(Jid), to_jid(J))
    end;
make_filter({_, _}) ->
    fun(_, _) -> true end.

match_jids(undefined, _) ->
    false;
match_jids(Jid, Pat) ->
    Jidb = to_binary(Jid),
    Patb = to_binary(Pat),
    case binary:match(Jidb, Patb) of
        {0, _} -> true;
        _ -> false
    end.


to_jid(undefined) ->
    undefined;
to_jid(J) when is_binary(J) ->
    jid:from_binary(J);
to_jid(#jid{} = J) ->
    J.


to_list(J) ->
    binary_to_list(to_binary(J)).

to_binary(#jid{} = J) ->
    jid:to_binary(J);
to_binary(J) ->
    J.

untrace(Name, Host, T) ->
    T1 = case maps:get(Name, T, undefined) of
             {_, Fo, Fi} ->
                 ejabberd_hooks:delete(user_send_packet, Host, Fo, 1),
                 ejabberd_hooks:delete(xmpp_send_element, Host, Fi, 1),
                 maps:remove(Name, T);
             undefined ->
                 T
         end,
    T1.

untrace_all(Host, T) ->
    untrace_all(Host, T,maps:keys(T)).

untrace_all(_Host, _T, []) ->
    ok;
untrace_all(Host, T, [Name|Keys]) ->
    untrace_all(Host, untrace(Name, Host, T), Keys).

format_message(Name, M) ->
    format_message(Name, lists:flatten(exml:to_pretty_iolist(M)), []).

format_message(_, [], R) ->
    R;
format_message(Name, [10|Tail], R) ->
    format_message(Name, Tail, R ++ io_lib:format("~n|~p|", [Name]));
format_message(Name, [C|Tail], R) ->
    format_message(Name, Tail, R ++ [C]).
