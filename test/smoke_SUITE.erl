%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet

-module(smoke_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([
	smoke/1
]).

all() ->
	[
		{group, default},
		{group, crypto_md5},
		{group, crypto_sha},
		{group, erlang_phash2}
	].

groups() ->
	Tests = [
		smoke
	],
	[
		{default, [parallel], Tests},
		{crypto_md5, [parallel], Tests},
		{crypto_sha, [parallel], Tests},
		{erlang_phash2, [parallel], Tests}
	].

init_per_suite(Config) ->
	ok = application:start(crypto),
	ok = application:start(ryng),
	Config.

end_per_suite(_Config) ->
	application:stop(ryng),
	application:stop(crypto),
	ok.

init_per_group(Name, Config) ->
	{Ring, RingConfig} = config_for_group(Name),
	ct:log("starting ~s ring...", [Name]),
	{ok, _Pid} = ryng:new_ring(RingConfig),
	ok = ryng:add_node(Ring, node0),
	ok = ryng:add_node(Ring, node1),
	ok = ryng:add_node(Ring, node2),
	ok = ryng:add_node(Ring, node3, 2),
	ok = ryng:sync_ring(Ring),
	ct:log("started"),
	[{ring, Ring} | Config].

end_per_group(Name, Config) ->
	Ring = ?config(ring, Config),
	ct:log("stopping ~s server...", [Name]),
	ryng:rm_ring(Ring),
	ct:log("stopped"),
	ok.

%%====================================================================
%% Tests
%%====================================================================

smoke(Config) ->
	Ring = ?config(ring, Config),
	{ok, Summary} = ryng:balance_summary(Ring),
	ct:log("[4 nodes with weights 0, 0, 0, 2] summary: ~p~n", [Summary]),
	N0 = proplists:get_value(node0, Summary),
	N1 = proplists:get_value(node1, Summary),
	N2 = proplists:get_value(node2, Summary),
	N3 = proplists:get_value(node3, Summary),
	true = N0 >= 0.16 orelse N0 < 0.17,
	true = N1 >= 0.16 orelse N1 < 0.17,
	true = N2 >= 0.16 orelse N2 < 0.17,
	true = N3 >= 0.49 orelse N3 < 0.51,
	{ok, _Timing, Balance} = ryng:balance_check(Ring, 10000),
	ct:log("[4 nodes with weights 0, 0, 0, 2] balance: ~p~n", [Balance]),
	B0 = element(3, lists:keyfind(node0, 1, Balance)),
	B1 = element(3, lists:keyfind(node1, 1, Balance)),
	B2 = element(3, lists:keyfind(node2, 1, Balance)),
	B3 = element(3, lists:keyfind(node3, 1, Balance)),
	true = B0 >= 0.16 orelse B0 < 0.17,
	true = B1 >= 0.16 orelse B1 < 0.17,
	true = B2 >= 0.16 orelse B2 < 0.17,
	true = B3 >= 0.49 orelse B3 < 0.51,
	ok = ryng:del_node(Ring, node3),
	ok = ryng:sync_ring(Ring),
	{ok, Summary2} = ryng:balance_summary(Ring),
	ct:log("[3 nodes with weights 0, 0, 0] summary: ~p~n", [Summary2]),
	M0 = proplists:get_value(node0, Summary2),
	M1 = proplists:get_value(node1, Summary2),
	M2 = proplists:get_value(node2, Summary2),
	true = M0 >= 0.32 orelse M0 < 0.34,
	true = M1 >= 0.32 orelse M1 < 0.34,
	true = M2 >= 0.32 orelse M2 < 0.34,
	{ok, _Timing2, Balance2} = ryng:balance_check(Ring, 10000),
	ct:log("[3 nodes with weights 0, 0, 0] balance: ~p~n", [Balance2]),
	C0 = element(3, lists:keyfind(node0, 1, Balance2)),
	C1 = element(3, lists:keyfind(node1, 1, Balance2)),
	C2 = element(3, lists:keyfind(node2, 1, Balance2)),
	true = C0 >= 0.32 orelse C0 < 0.34,
	true = C1 >= 0.32 orelse C1 < 0.34,
	true = C2 >= 0.32 orelse C2 < 0.34,
	ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% @private
config_for_group(default) ->
	{default_ring, [
		{name, default_ring}
	]};
config_for_group(crypto_md5) ->
	{crypto_md5_ring, [
		{name, crypto_md5_ring},
		{hash, md5}
	]};
config_for_group(crypto_sha) ->
	{crypto_sha_ring, [
		{name, crypto_sha_ring},
		{hash, sha}
	]};
config_for_group(erlang_phash2) ->
	{erlang_phash2_ring, [
		{name, erlang_phash2_ring},
		{hash, {erlang, phash2}},
		{bits, 27}
	]}.
