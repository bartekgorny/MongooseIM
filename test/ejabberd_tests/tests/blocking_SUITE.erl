%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

-module(blocking_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(NS_BLOCKING,     <<"urn:xmpp:blocking">>).

-define(SLEEP_TIME, 50).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, base}
    ].

groups() ->
    [{base, [sequence], base_test_cases()}
    ].

base_test_cases() ->
    [
        discovering_support,
        get_block_list,
        add_user_to_blocklist,
        add_another_user_to_blocklist,
        add_many_users_to_blocklist,
        remove_user_from_blocklist,
        remove_many_user_from_blocklist,
        clear_blocklist,
        invalid_block_request
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    [{escalus_no_stanzas_after_story, true} |
     escalus:init_per_suite(Config)].

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob, carol, mike, geralt])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, carol, mike, geralt])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

discovering_support(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(User1) ->
            Server = escalus_client:server(User1),
            IqGet = escalus_stanza:disco_info(Server),
            escalus_client:send(User1, IqGet),
            Result = escalus_client:wait_for_stanza(User1),
            escalus:assert(is_iq_result, [IqGet], Result),
            escalus:assert(has_feature, [?NS_BLOCKING], Result)
        end).


get_block_list(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(User1) ->
            Result = get_blocklist(User1),
            io:format("Received stanza is ~n~p~n",[Result]),
            ct:pal("RESULT ~p", [Result]),
            escalus:assert(is_iq_result, Result),
            escalus:assert(fun is_blocklist_result_empty/1, Result)
        end).


add_user_to_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2]),
            BlockList = get_blocklist(User1),
            blocklist_contains_jid(BlockList, User2)
        end).

add_another_user_to_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {mike, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2]),
            BlockList = get_blocklist(User1),
            blocklist_contains_jid(BlockList, User2)
        end).

add_many_users_to_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {carol, 1}, {mike,1}],
        fun(User1, User2, User3, User4) ->
            user_blocks(User1, [User2, User3, User4]),
            BlockList = get_blocklist(User1),
            blocklist_contains_jid(BlockList, User2),
            blocklist_contains_jid(BlockList, User3),
            blocklist_contains_jid(BlockList, User4)
        end).

remove_user_from_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}],
        fun(User1, User2) ->
            user_blocks(User1, [User2]),
            user_unblocks(User1, User2),
            NewList = get_blocklist(User1),
            blocklist_doesnt_contain_jid(NewList, User2)
        end).

remove_many_user_from_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {geralt, 1}],
        fun(User1, User2, User3) ->
            user_blocks(User1, [User2, User3]),
            user_unblocks(User1, [User2, User3]),
            NewList = get_blocklist(User1),
            blocklist_doesnt_contain_jid(NewList, User2),
            blocklist_doesnt_contain_jid(NewList, User3)
        end).

clear_blocklist(Config) ->
    escalus:story(
        Config, [{alice, 1}, {bob, 1}, {geralt, 1}],
        fun(User1, User2, User3) ->
            user_blocks(User1, [User2, User3]),
            user_unblocks_all(User1),
            NewList = get_blocklist(User1),
            blocklist_is_empty(NewList)
        end).

invalid_block_request(Config) ->
    escalus:story(
        Config, [{alice, 1}],
        fun(User1) ->
            St = block_users_stanza([]),
            escalus_client:send(User1, St),
            Stanza = escalus_client:wait_for_stanza(User1),
            ct:pal("badrequest ~p", [Stanza]),
            escalus_assert:is_error(Stanza, <<"modify">>, <<"bad-request">>)
        end).


%% common
%%
get_blocklist(User) ->
    IQGet = get_blocklist_stanza(),
    ct:pal("SEND ~p", [IQGet]),
    escalus_client:send(User, IQGet),
    escalus_client:wait_for_stanza(User).

%%
%% stanza generators
%%

get_blocklist_stanza() ->
    Payload = #xmlel{name = <<"blocklist">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}]},
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"get">>}],
        children = [Payload]}.

block_users_stanza(UsersToBlock) ->
    Childs = [item_el(U) || U <- UsersToBlock],
    Payload = #xmlel{name = <<"block">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
        children = Childs
    },
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"set">>}],
        children = [Payload]}.


%%block_user_stanza(UserToBlock) ->
%%    Payload = #xmlel{name = <<"block">>,
%%        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
%%        children = [item_el(UserToBlock)]
%%    },
%%    #xmlel{name = <<"iq">>,
%%        attrs = [{<<"type">>, <<"set">>}],
%%        children = Payload}.

unblock_user_stanza(UserToUnblock) ->
    Payload = #xmlel{name = <<"unblock">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
        children = [item_el(UserToUnblock)]
    },
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"set">>}],
        children = [Payload]}.

unblock_users_stanza(UsersToBlock) ->
    Childs = [item_el(U) || U <- UsersToBlock],
    Payload = #xmlel{name = <<"unblock">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
        children = Childs
    },
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"set">>}],
        children = [Payload]}.

unblock_all_stanza() ->
    Payload = #xmlel{name = <<"unblock">>,
        attrs=[{<<"xmlns">>, ?NS_BLOCKING}],
        children = []
    },
    #xmlel{name = <<"iq">>,
        attrs = [{<<"type">>, <<"set">>}],
        children = [Payload]}.

item_el(User) when is_binary(User) ->
    #xmlel{name = <<"item">>,
        attrs = [{<<"jid">>, User}]}.
%%
%% predicates
%%

is_xep191_not_available(#xmlel{} = Stanza) ->
    ErrorEl = exml_query:subelement(Stanza, <<"error">>),
    <<"error">> == exml_query:attr(Stanza, <<"type">>)
        andalso
        undefined =/= exml_query:subelement(ErrorEl, <<"not-acceptable">>)
        andalso
        undefined =/= exml_query:subelement(ErrorEl, <<"blocked">>)
        andalso
        <<"urn:xmpp:blocking:errors">> ==
            exml_query:path(ErrorEl, [{element, <<"blocked">>},
                {attr, <<"xmlns">>}]).


is_blocklist_result_empty(#xmlel{children = [#xmlel{name =Name,
    attrs = Attrs,
    children= Child}]} = Stanza) ->
    true = escalus_pred:is_iq(Stanza),
    <<"blocklist">> = Name,
    {<<"xmlns">>, ?NS_BLOCKING} = lists:keyfind(<<"xmlns">>, 1, Attrs),
    [] = Child,
    true.

blocklist_result_has(ExpectedUser, Stanza) ->
    true = escalus_pred:is_iq(Stanza),
    Blocklist = hd(Stanza#xmlel.children),
    Attrs = Blocklist#xmlel.attrs,
    Children = Blocklist#xmlel.children,
    <<"blocklist">> = Blocklist#xmlel.name,
    {<<"xmlns">>, ?NS_BLOCKING} = lists:keyfind(<<"xmlns">>, 1, Attrs),
    true == lists:member(ExpectedUser,get_blocklist_items(Children)).

is_xep191_push(Type, #xmlel{attrs = A, children = [#xmlel{name = Type,
    attrs = Attrs}]}=Stanza) ->
    true = escalus_pred:is_iq_set(Stanza),
    {<<"id">>, <<"push">>} = lists:keyfind(<<"id">>, 1, A),
    {<<"xmlns">>, ?NS_BLOCKING} = lists:keyfind(<<"xmlns">>, 1, Attrs),
    true.

is_xep191_push(Type, JIDs, #xmlel{attrs = A, children = [#xmlel{name = Type,
    attrs = Attrs, children = Items}]}=Stanza) ->
    true = escalus_pred:is_iq_set(Stanza),
    {<<"xmlns">>, ?NS_BLOCKING} = lists:keyfind(<<"xmlns">>, 1, Attrs),
    F = fun(El) ->
        #xmlel{name = <<"item">>, attrs =  [{<<"jid">>, Value}]} = El,
        lists:member(Value, JIDs)
        end,
    TrueList = lists:map(F, Items),
    lists:all(fun(El) -> El end, TrueList);
is_xep191_push(_, _, _) ->
    false.

%%
%% helpers
%%

bare(C) ->  escalus_utils:jid_to_lower(escalus_client:short_jid(C)).

get_blocklist_items(Items) ->
    lists:map(fun(#xmlel{name = <<"item">>, attrs=A}) ->
        {_, R} = lists:keyfind(<<"jid">>, 1, A),
        R
              end, Items).

user_blocks(Blocker, Blockees) when is_list(Blockees) ->
    BlockeeJIDs = [ escalus_utils:jid_to_lower(escalus_client:short_jid(B)) || B <- Blockees ],
    AddStanza = block_users_stanza(BlockeeJIDs),
    ct:pal("add stanza ~p", [AddStanza]),
    escalus_client:send(Blocker, AddStanza),
    Res = escalus:wait_for_stanzas(Blocker, 2),
    CheckPush = fun(E) -> is_xep191_push(<<"block">>, BlockeeJIDs, E) end,
    Preds = [is_iq_result, CheckPush],
    escalus:assert_many(Preds, Res).

blocklist_is_empty(BlockList) ->
    escalus:assert(is_iq_result, BlockList),
    escalus:assert(fun is_blocklist_result_empty/1, BlockList).

blocklist_contains_jid(BlockList, Client) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
    escalus:assert(fun blocklist_result_has/2, [JID], BlockList).

user_unblocks(Unblocker, Unblockees) when is_list(Unblockees) ->
    UnblockeeJIDs = [ escalus_utils:jid_to_lower(escalus_client:short_jid(B)) || B <- Unblockees ],
    AddStanza = unblock_users_stanza(UnblockeeJIDs),
    escalus_client:send(Unblocker, AddStanza),
    Res = escalus:wait_for_stanzas(Unblocker, 2),
    CheckPush = fun(E) -> is_xep191_push(<<"unblock">>, UnblockeeJIDs, E) end,
    Preds = [is_iq_result, CheckPush],
    escalus:assert_many(Preds, Res);
user_unblocks(Unblocker, Unblockee) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Unblockee)),
    escalus_client:send(Unblocker, unblock_user_stanza(JID)),
    user_gets_remove_result(Unblocker).

blocklist_doesnt_contain_jid(BlockList, Client) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Client)),
    escalus:assert(is_iq_result, BlockList),
    ?assertNot(blocklist_result_has(JID, BlockList)).

user_gets_remove_result(Client) ->
    RemoveResult = escalus:wait_for_stanzas(Client, 2),
    CheckPush = fun(E) -> is_xep191_push(<<"unblock">>, E) end,
    Preds = [is_iq_result, CheckPush],
    escalus:assert_many(Preds, RemoveResult).


user_unblocks_all(User) ->
    escalus_client:send(User, unblock_all_stanza()),
    user_gets_remove_result(User).

