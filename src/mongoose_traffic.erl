-module(mongoose_traffic).

%% The most simple use case possible is:
%% - add {mongoose_debug, []} to modules in mongooseim.cfg
%% - from erlang shell, run recon_trace:calls([{mongoose_debug, traffic, '_'}], 100, [{scope, local}]).
%% - watch all the traffic coming in and out

-behaviour(gen_mod).
-behaviour(gen_server).

-include("mongoose.hrl").
-include("jlib.hrl").

%% API
-export([start/2, stop/1]).
-export([trace_traffic/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {traces = #{}, traceall = false}).

-define(MAX_ITEMS, 500).

start(Host, _Opts) ->
    % TODO make it a service
    ejabberd_hooks:add(hooks(Host)),
    Traffic = {mongoose_traffic,
                 {gen_server, start_link, [?MODULE, [], []]},
                 permanent, 1000, supervisor, [?MODULE]},
    % there has to be another layer
    % channel will set up its own traces, someone has to watch and distribute stanzas
    ejabberd_sup:start_child(Traffic),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

hooks(Host) ->
    [{c2s_debug, Host, ?MODULE, trace_traffic, 50}].


trace_traffic(Acc, {out, From, El}) ->
    traffic(out, From, El),
    Acc;
trace_traffic(Acc, {in, {_From, To, El}}) ->
    traffic(in, To, El),
    Acc.

traffic(Dir, Account, El) ->
    Sacc = jid:to_binary(Account),
    St = exml:to_pretty_iolist(El),
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> gen_server:cast(Pid, {message, Dir, Sacc, St})
    end,
    ok.



%%%===================================================================
%%% API
%%%===================================================================

init([]) ->
    register(?MODULE, self()),
    {ok, #state{}}.

handle_call({trace, Jid}, _From, State) ->
    {reply, ok, State#state{traces =  add_trace(to_binary(Jid), State#state.traces)}};
handle_call({untrace, Jid}, _From, State) ->
    {reply, ok, State#state{traces =  remove_trace(to_binary(Jid), State#state.traces)}};
handle_call({clear, Jid}, _From, State) ->
    {reply, ok, State#state{traces =  clear_trace(to_binary(Jid), State#state.traces)}};
handle_call(clear_all, _From, State) ->
    {reply, ok, State#state{traces =  clear_all(State#state.traces)}};
handle_call(untrace_all, _From, State) ->
    {reply, ok, State#state{traces = #{}}};
handle_call({traceall, Flag}, _From, State) ->
    {reply, ok, State#state{traceall = Flag}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({message, Dir, J, Stanza}, State) ->
    case is_traced(J, State) of
        true ->
            publish_message(J, Dir, Stanza),
            {noreply, State#state{traces = record_item(Dir, J, Stanza, State#state.traces)}};
        false ->
            {noreply, State}
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

to_binary(#jid{} = Jid) -> jid:to_binary(Jid);
to_binary(Jid) -> Jid.

add_trace(J, Traces) ->
    case maps:get(J, Traces, none) of
        none -> maps:put(J, queue:new(), Traces);
        _ -> Traces
    end.

remove_trace(J, Traces) -> maps:remove(J, Traces).

clear_trace(J, Traces) -> maps:put(J, queue:new(), Traces).

clear_all(Traces) -> lists:foldl(fun(J, T) -> maps:put(J, queue:new(), T) end, #{}, maps:keys(Traces)).

is_traced(_, #state{traceall = true}) -> true;
is_traced(J, #state{traces = Traces}) -> maps:is_key(J, Traces).

record_item(Dir, J, Stanza, Traces) ->
    Tr = case maps:get(J, Traces, undefined) of
             undefined ->
                 announce_new_trace(J),
                 queue:new();
             Q -> Q
         end,
    Tr1 = queue:in({Dir, Stanza}, Tr),
    Tr2 = case queue:len(Tr1) of
              ?MAX_ITEMS -> queue:out(Tr1);
              _ -> Tr1
          end,
    maps:put(J, Tr2, Traces).

publish_message(_J, _Dir, _Stanza) ->
    % somehow send to an appropriate channel
    ok.

announce_new_trace(_J) ->
    % so that you know there is something to watch (if you are tracing all)
    ok.
