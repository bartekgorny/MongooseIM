%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(feature_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(ne(E, I), ?assert(E =/= I)).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, order}].

groups() ->
    [{order, [], [stream_compression_without_tls, stream_compression_after_tls]}].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    escalus:create_users(Config1, {by_name, [secure_joe]}).

end_per_suite(Config0) ->
    Config1 = escalus:delete_users(Config0, {by_name, [secure_joe]}),
    escalus:end_per_suite(Config1).

init_per_group(order, Config) ->
    Config.

end_per_group(order, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Connection steps test
%%--------------------------------------------------------------------
stream_compression_without_tls(Config) ->
    Joe = escalus_users:get_options(Config, secure_joe),
    {ok, Conn, _, _} = escalus_connection:start(Joe, [start_stream,
                                                      stream_features,
                                                      maybe_use_compression,
                                                      authenticate, bind, session]),
    SSL = Conn#client.compress,
    ?ne(SSL, false).

stream_compression_after_tls(Config) ->
    Joe = escalus_users:get_options(Config, secure_joe),
    {ok, Conn, _, _} = escalus_connection:start(Joe, [start_stream,
                                                      stream_features,
                                                      maybe_use_ssl,            %% added this line
                                                      maybe_use_compression,
                                                      authenticate, bind, session]),
    SSL = Conn#client.compress,
    ?ne(SSL, false).