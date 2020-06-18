-module(mongoose_traffic).

%% The most simple use case possible is:
%% - add {mongoose_debug, []} to modules in mongooseim.cfg
%% - from erlang shell, run recon_trace:calls([{mongoose_debug, traffic, '_'}], 100, [{scope, local}]).
%% - watch all the traffic coming in and out

-behaviour(gen_mod).
-behaviour(gen_server).

-include("mongoose.hrl").
-include("jlib.hrl").

%% gen_mod API
-export([start/2, stop/1]).
%% hook handler
-export([trace_traffic/2]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
%% cowboy handler for serving main page
-export([init/2]).

-define(SERVER, ?MODULE).


start(Host, _Opts) ->
    ejabberd_hooks:add(hooks(Host)),
    case whereis(?MODULE) of
        undefined ->
            Traffic = {mongoose_traffic,
                         {gen_server, start_link, [?MODULE, [], []]},
                         permanent, 1000, supervisor, [?MODULE]},
            % there has to be another layer
            % channel will set up its own traces, someone has to watch and distribute stanzas
            ejabberd_sup:start_child(Traffic);
        _ ->
            ok
    end,
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
trace_traffic(Acc, {in, El}) ->
    To = case get_jid(stanza, El) of
             undefined -> get_jid(acc, Acc);
             J -> jid:from_binary(J)
         end,
    traffic(in, To, El),
    Acc.

traffic(Dir, Jid, El) ->
    St = fix_and_format(El),
    UserPid = self(),
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> gen_server:cast(Pid, {message, Dir, {UserPid, Jid}, St})
    end,
    ok.


init([]) ->
    register(?MODULE, self()),
    {ok, []}.

handle_call({register, Pid}, _From, State) ->
    monitor(process, Pid),
    {reply, ok, [Pid | State]};
handle_call({unregister, Pid}, _From, State) ->
    {reply, ok, lists:delete(Pid, State)};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({message, _, _, _} = Msg, State) ->
    lists:map(fun(Pid) -> Pid ! Msg end, State),
    {noreply, State}.

handle_info({'DOWN', _, _, Pid, _}, State) ->
    {noreply, lists:delete(Pid, State)}.

init(Req, State) ->
    {ok, Cwd} = file:get_cwd(),
    Base = Cwd ++ "/web/traffic",
    File = case cowboy_req:path_info(Req) of
               [] -> "session.html";
               P -> filename:join(P)
           end,
    Path = filename:join(Base, File),
    Size = filelib:file_size(Path),
    Req1 = cowboy_req:reply(200,
                            #{},
                            {sendfile, 0, Size, Path}, Req),
    {ok, Req1, State}.

fix_and_format(El) when is_binary(El) ->
    El;
fix_and_format({Tag, Name, Attrs}) ->
    exml:to_pretty_iolist({Tag, Name, fix_attrs(Attrs)});
fix_and_format({Tag, Name, Attrs, Children}) ->
    exml:to_pretty_iolist({Tag, Name, fix_attrs(Attrs), Children}).

fix_attrs(Attrs) ->
    lists:filter(fun is_defined/1, Attrs).

is_defined({_, undefined}) -> false;
is_defined({_, _}) -> true.

get_jid(stanza, #xmlel{} = El) ->
    exml_query:attr(El, <<"to">>);
get_jid(stanza, _) ->
    undefined;
get_jid(acc, no_acc) ->
    undefined;
get_jid(acc, Acc) ->
    mongoose_acc:to_jid(Acc).
