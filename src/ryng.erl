%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  09 Sep 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(ryng).
-behaviour(gen_server).

-include("ryng.hrl").

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(TAB_RINGS, ryng_rings).

%% API
-export([manual_start/0, start_link/0, int_to_bin/1, int_to_bin/2, bin_to_int/1]).

%% Ring API
-export([list_rings/0, new_ring/1, rm_ring/1, delete_ring/1, get_ring/1,
	is_ring/1, sync_ring/1, refresh_ring/1, set_ring/2]).

%% Node API
-export([list_nodes/1, add_node/2, add_node/3, del_node/2, is_node/2]).

%% Object API
-export([balance_check/2, balance_summary/1, hash_for/2, index_for/2,
	key_for/2, key_of/1, node_for/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% Internal
-export([hasher/1]).

-type monitors() :: [{{reference(), pid()}, any()}].
-record(state, {
	monitors = [] :: monitors()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Manually start ryng and all dependencies.
-spec manual_start() -> ok.
manual_start() ->
	require([crypto, ryng]).

%% @private
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Convert an Erlang integer to a binary representation of an integer.
-spec int_to_bin(integer()) -> bitstring().
int_to_bin(X) when is_integer(X) andalso X < 0 ->
	int_to_bin_neg(X, []);
int_to_bin(X) when is_integer(X) ->
	int_to_bin_pos(X, []);
int_to_bin(X) ->
	erlang:error(badarg, [X]).

%% @doc Convert an Erlang integer to the binary representation of Bits length.
-spec int_to_bin(integer(), integer()) -> bitstring().
int_to_bin(X, Bits) when is_integer(X) andalso X < 0 andalso is_integer(Bits) andalso Bits >= 0 ->
	case int_to_bin_neg(X, Bits, <<>>) of
		badarg ->
			erlang:error(badarg, [X, Bits]);
		B ->
			B
	end;
int_to_bin(X, Bits) when is_integer(X) andalso is_integer(Bits) andalso Bits >= 0 ->
	case int_to_bin_pos(X, Bits, <<>>) of
		badarg ->
			erlang:error(badarg, [X, Bits]);
		B ->
			B
	end;
int_to_bin(X, Bits) ->
	erlang:error(badarg, [X, Bits]).

%% @doc Convert binary representation, of an integer, to an Erlang integer.
-spec bin_to_int(bitstring()) -> integer().
bin_to_int(Bin) when is_bitstring(Bin) ->
	Bits = bit_size(Bin),
	<<Integer:Bits/integer>> = Bin,
	Integer;
bin_to_int(X) ->
	erlang:error(badarg, [X]).

%%%===================================================================
%%% Ring API functions
%%%===================================================================

list_rings() ->
	{ok, ets:match_object(?TAB_RINGS, '_')}.

new_ring(RingConfig) ->
	ryng_sup:new_ring(RingConfig).

rm_ring(RingName) ->
	ryng_sup:rm_ring(RingName).

delete_ring(RingName) ->
	ryng_sup:delete_ring(RingName).

get_ring(RingName) ->
	case ets:lookup(?TAB_RINGS, RingName) of
		[Ring=#ring{}] ->
			{ok, Ring};
		_ ->
			{error, ring_not_found}
	end.

is_ring(RingName) ->
	ets:member(?TAB_RINGS, RingName).

sync_ring(RingName) ->
	gen_server:call(RingName, sync_ring).

%% @private
refresh_ring(RingName) ->
	case get_ring(RingName) of
		{ok, Ring=#ring{name=RingName, max=Max}} ->
			case list_nodes(RingName) of
				{ok, Nodes} ->
					Size = lists:sum([W+1 || #node{weight=W} <- Nodes]),
					Inc = case Size of
						0 ->
							Max;
						_ ->
							Max div Size
					end,
					Ring2 = Ring#ring{max=Max, inc=Inc, size=Size},
					case gen_server:call(?SERVER, {refresh_ring, Ring2, self()}) of
						true ->
							{ok, Ring2};
						false ->
							{error, not_ring_owner}
					end;
				NodesError ->
					NodesError
			end;
		RingError ->
			RingError
	end.

%% @private
set_ring(RingName, Pid) ->
	gen_server:call(?SERVER, {set_ring, RingName, Pid}).

%%%===================================================================
%%% Node API functions
%%%===================================================================

list_nodes(RingName) ->
	ryng_ring:list_nodes(RingName).

add_node(RingName, NodeObject) ->
	add_node(RingName, NodeObject, 0).

add_node(RingName, NodeObject, NodeWeight) ->
	ryng_ring:add_node(RingName, NodeObject, NodeWeight).

del_node(RingName, NodeObject) ->
	ryng_ring:del_node(RingName, NodeObject).

is_node(RingName, NodeObject) ->
	ryng_ring:is_node(RingName, NodeObject).

%%%===================================================================
%%% Object API functions
%%%===================================================================

balance_check(RingName, Iterations) when is_integer(Iterations) andalso Iterations > 0 ->
	case node_for(RingName, 0) of
		{ok, _} ->
			Start = erlang:now(),
			Results = [begin
				{ok, NodeObject} = node_for(RingName, Iteration),
				NodeObject
			end || Iteration <- lists:seq(1, Iterations)],
			Stop = erlang:now(),
			Balance = balance_count(Results),
			BalanceAverage = [{NodeObject, Count, Count / Iterations} || {NodeObject, Count} <- Balance],
			TimeTotal = timer:now_diff(Stop, Start),
			TimeAverage = TimeTotal / Iterations,
			{ok, {Iterations, TimeTotal, TimeAverage}, BalanceAverage};
		NodeError ->
			NodeError
	end.

balance_summary(RingName) ->
	case get_ring(RingName) of
		{ok, #ring{name=RingName}} ->
			case list_nodes(RingName) of
				{ok, Nodes} ->
					Size = lists:sum([W+1 || #node{weight=W} <- Nodes]),
					Balance = [{NodeObject, (NodeWeight + 1) / Size} || #node{object=NodeObject, weight=NodeWeight} <- Nodes],
					{ok, Balance};
				NodesError ->
					NodesError
			end;
		RingError ->
			RingError
	end.

hash_for(RingName, Binary) when is_binary(Binary) ->
	ryng_ring:hash_for(RingName, Binary).

index_for(RingName, Binary) when is_binary(Binary) ->
	ryng_ring:index_for(RingName, Binary).

key_for(RingName, Object) ->
	ryng_ring:key_for(RingName, Object).

key_of(Object) ->
	ryng_ring:key_of(Object).

node_for(RingName, Object) ->
	ryng_ring:node_for(RingName, Object).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
	ok = ryng_event:add_handler(ryng_event_handler, self()),
	?TAB = ryng_ets:soft_new(?TAB, [
		named_table,
		protected,
		ordered_set
	]),
	?TAB_RINGS = ryng_ets:soft_new(?TAB_RINGS, [
		named_table,
		protected,
		{keypos, #ring.name},
		{write_concurrency, false},
		{read_concurrency, true}
	]),
	Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} || [Ref, Pid] <- ets:match(?TAB, {{ring, '$1'}, '$2'})],
	{ok, #state{monitors=Monitors}}.

%% @private
handle_call({set_ring, Ring=#ring{name=Ref, hash=Hash, bits=Bits}, Pid}, _From, State=#state{monitors=Monitors}) ->
	case ets:insert_new(?TAB, {{ring, Ref}, Pid}) of
		true ->
			MonitorRef = erlang:monitor(process, Pid),
			State2 = State#state{monitors=[{{MonitorRef, Pid}, Ref} | Monitors]},
			try
				Hasher = hasher(Hash),
				Bits2 = case Bits of
					undefined ->
						bit_size(Hasher(<<>>));
					_ when is_integer(Bits) ->
						Bits
				end,
				Max = trunc(math:pow(2, Bits2) - 1),
				Inc = Max,
				Size = 0,
				Ring2 = Ring#ring{bits=Bits2, hasher=Hasher, max=Max, inc=Inc, size=Size},
				true = ets:insert(?TAB_RINGS, Ring2),
				ryng_event:ring_add(Ring2),
				{reply, true, State2}
			catch
				Class:Reason ->
					error_logger:error_msg(
						"** ~p ~p non-fatal ring error in ~p/~p~n"
						"** Error was: ~p:~p~n** Ring was: ~p~n"
						"** Stacktrace: ~p~n~n",
						[?MODULE, self(), handle_call, 3, Class, Reason, Ring, erlang:get_stacktrace()]),
					{reply, false, State2}
			end;
		false ->
			{reply, false, State}
	end;
handle_call({refresh_ring, Ring=#ring{name=Ref}, Pid}, _From, State) ->
	case catch ets:lookup_element(?TAB, {ring, Ref}, 2) of
		Pid ->
			true = ets:insert(?TAB_RINGS, Ring),
			ryng_event:ring_refresh(Ring),
			{reply, true, State};
		_ ->
			{reply, false, State}
	end;
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'ETS-TRANSFER', Table, _Pid, []}, State) when Table =:= ?TAB orelse Table =:= ?TAB_RINGS ->
	{noreply, State};
handle_info({'DOWN', MonitorRef, process, Pid, _}, State=#state{monitors=Monitors}) ->
	{_, Ref} = lists:keyfind({MonitorRef, Pid}, 1, Monitors),
	true = ets:delete(?TAB, {ring, Ref}),
	true = ets:delete(?TAB_RINGS, Ref),
	Monitors2 = lists:keydelete({MonitorRef, Pid}, 1, Monitors),
	{noreply, State#state{monitors=Monitors2}};
handle_info({'$ryng', {ring, del, RingName}}, State) ->
	ok = ryng:delete_ring(RingName),
	{noreply, State};
handle_info({'$ryng', _Event}, State) ->
	{noreply, State};
handle_info(Info, State) ->
	error_logger:error_msg(
		"** ~p ~p unhandled info in ~p/~p~n"
		"   Info was: ~p~n",
		[?MODULE, self(), handle_info, 2, Info]),
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
balance_count(Results) when is_list(Results) ->
	balance_count(Results, dict:new()).

%% @private
balance_count([], Balance) ->
	dict:to_list(Balance);
balance_count([NodeObject | NodeObjects], Balance) ->
	balance_count(NodeObjects, dict:update_counter(NodeObject, 1, Balance)).

%% @private
hasher(Algorithm) when is_atom(Algorithm) ->
	fun(Binary) ->
		crypto:hash(Algorithm, Binary)
	end;
hasher(Function) when is_function(Function, 1) ->
	Function;
hasher({Function, Arguments}) when is_function(Function) andalso is_list(Arguments) ->
	fun(Binary) ->
		erlang:apply(Function, Arguments ++ [Binary])
	end;
hasher({Module, Function}) when is_atom(Module) andalso is_atom(Function) ->
	fun Module:Function/1;
hasher({Module, Function, Arguments}) when is_atom(Module) andalso is_atom(Function) andalso is_list(Arguments) ->
	fun(Binary) ->
		erlang:apply(Module, Function, Arguments ++ [Binary])
	end.

%% @private
int_to_bin_pos(0,Ds=[_|_]) ->
	list_to_binary(Ds);
int_to_bin_pos(X,Ds) ->
	int_to_bin_pos(X bsr 8, [(X band 255)|Ds]).

%% @private
int_to_bin_pos(0, 0, D) ->
	D;
int_to_bin_pos(0, B, D) when B > 0 ->
	<< 0:B, D/binary >>;
int_to_bin_pos(X, B, D) when B < 8 andalso (X bsr B) =:= 0 ->
	int_to_bin_pos(X bsr B, B - B, << (X band ((2 bsl (B - 1)) - 1)):B, D/binary >>);
int_to_bin_pos(X, B, D) when B >= 8 ->
	int_to_bin_pos(X bsr 8, B - 8, << (X band 255), D/binary >>);
int_to_bin_pos(_X, _B, _D) ->
	badarg.

%% @private
int_to_bin_neg(-1, Ds=[MSB|_]) when MSB >= 16#80 ->
	list_to_binary(Ds);
int_to_bin_neg(X,Ds) ->
	int_to_bin_neg(X bsr 8, [(X band 255)|Ds]).

%% @private
int_to_bin_neg(-1, B, <<>>) ->
	<< (-1):B >>;
int_to_bin_neg(-1, 0, D) ->
	D;
int_to_bin_neg(-1, B, D) when B > 0 ->
	<< (-1):B, D/binary >>;
int_to_bin_neg(X, B, D) when B < 8 andalso (X bsr B) =:= -1 ->
	int_to_bin_neg(X bsr B, B - B, << (X band ((2 bsl (B - 1)) - 1)):B, D/binary >>);
int_to_bin_neg(X, B, D) when B >= 8 ->
	int_to_bin_neg(X bsr 8, B - 8, << (X band 255), D/binary >>);
int_to_bin_neg(_X, _B, _D) ->
	badarg.

%% @doc Start the given applications if they were not already started.
%% @private
-spec require(list(module())) -> ok.
require([]) ->
	ok;
require([App|Tail]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Tail).
