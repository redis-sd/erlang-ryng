%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
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
-define(TAB_NODES, ryng_nodes).
-define(TAB_PTRS, ryng_ptrs).

%% API
-export([manual_start/0, start_link/0]).
-export([int_to_bin/1, int_to_bin/2, bin_to_int/1]).

%% Name Server API
-export([register_name/2, whereis_name/1, unregister_name/1, send/2]).

%% Ring API
-export([list_rings/0, new_ring/1, rm_ring/1, delete_ring/1, get_ring/1,
	is_ring/1, is_ring_empty/1, sync_ring/1]).
-export([nodes_table/1, ptrs_table/1, refresh_ring/1, resolve_ring/1]).

%% Node API
-export([list_nodes/1, add_node/2, add_node/3, add_node/4, del_node/2,
	get_node/2, is_node/2, set_node/4]).

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
%%% Name Server API functions
%%%===================================================================

-spec register_name(Name::term(), Pid::pid()) -> 'yes' | 'no'.
register_name(Name, Pid) when is_pid(Pid) ->
	gen_server:call(?SERVER, {register_name, Name, Pid}, infinity).

-spec whereis_name(Name::term()) -> pid() | 'undefined'.
whereis_name(Name) ->
	case ets:lookup(?TAB, {pid, Name}) of
		[{{pid, Name}, Pid}] ->
			case erlang:is_process_alive(Pid) of
				true ->
					Pid;
				false ->
					undefined
			end;
		[] ->
			undefined
	end.

-spec unregister_name(Name::term()) -> term().
unregister_name(Name) ->
	case whereis_name(Name) of
		undefined ->
			ok;
		_ ->
			_ = ets:delete(?TAB, {pid, Name}),
			ok
	end.

-spec send(Name::term(), Msg::term()) -> Pid::pid().
send(Name, Msg) ->
	case whereis_name(Name) of
		Pid when is_pid(Pid) ->
			Pid ! Msg,
			Pid;
		undefined ->
			erlang:error(badarg, [Name, Msg])
	end.

%%%===================================================================
%%% Ring API functions
%%%===================================================================

list_rings() ->
	{ok, ets:match_object(?TAB_RINGS, '_')}.

new_ring(RingConfig) ->
	ryng_sup:new_ring(RingConfig).

rm_ring(RingId) ->
	ryng_sup:rm_ring(RingId).

delete_ring(RingId) ->
	ryng_sup:delete_ring(RingId).

get_ring(RingId) ->
	case ets:lookup(?TAB_RINGS, RingId) of
		[Ring=?RYNG_RING{}] ->
			{ok, Ring};
		_ ->
			{error, ring_not_found}
	end.

is_ring(RingId) ->
	ets:member(?TAB_RINGS, RingId).

is_ring_empty(RingId) ->
	ryng_ring:is_empty(RingId).

sync_ring(RingId) ->
	gen_server:call({via, ryng, RingId}, sync_ring).

%% @private
nodes_table(RingId) ->
	ets:lookup_element(?TAB_RINGS, RingId, ?RYNG_RING.nodes).

%% @private
ptrs_table(RingId) ->
	ets:lookup_element(?TAB_RINGS, RingId, ?RYNG_RING.ptrs).

%% @private
refresh_ring(RingId) ->
	case is_ring_owner(RingId) of
		false ->
			{error, not_ring_owner};
		true ->
			case get_ring(RingId) of
				{ok, Ring=?RYNG_RING{id=RingId}} ->
					case list_nodes(RingId) of
						{ok, Nodes} ->
							Ring2 = refresh_ring_by_priority(Nodes, Ring, dict:new()),
							Reply = gen_server:call(?SERVER, {refresh_ring, Ring2}),
							{Reply, Ring2};
						ListNodesError ->
							ListNodesError
					end;
				GetRingError ->
					GetRingError
			end
	end.

%% @private
resolve_ring(Ring=?RYNG_RING{id=RingId, hash=Hash, bits=Bits}) ->
	case is_ring_owner(RingId) of
		false ->
			false;
		true ->
			case ets:lookup(?TAB_RINGS, RingId) of
				[OldRing=?RYNG_RING{id=RingId, hash=Hash, bits=Bits, nodes=Nodes, ptrs=Ptrs}] ->
					Nodes = ryng_ets:give_away(Nodes),
					Ptrs = ryng_ets:give_away(Ptrs),
					{true, OldRing};
				[?RYNG_RING{id=RingId, nodes=Nodes, ptrs=Ptrs}] ->
					catch ets:delete(Nodes),
					catch ets:delete(Ptrs),
					true = gen_server:call(?SERVER, {drop_ring, RingId}),
					resolve_new_ring(Ring);
				[] ->
					resolve_new_ring(Ring)
			end
	end.

%% @private
resolve_new_ring(Ring=?RYNG_RING{nodes=undefined, ptrs=undefined, hash=Hash, bits=Bits}) ->
	Nodes = ryng_ets:new({?TAB_NODES, erlang:make_ref()}, [
		protected,
		ordered_set,
		{keypos, ?RYNG_NODE.object},
		{write_concurrency, false},
		{read_concurrency, true}
	]),
	Ptrs = ryng_ets:new({?TAB_PTRS, erlang:make_ref()}, [
		protected,
		ordered_set,
		{keypos, ?RYNG_PTR.index},
		{write_concurrency, false},
		{read_concurrency, true}
	]),
	Hasher = hasher(Hash),
	Bits2 = case Bits of
		undefined ->
			bit_size(Hasher(<<>>));
		_ when is_integer(Bits) ->
			Bits
	end,
	Max = trunc(math:pow(2, Bits2) - 1),
	Incrs = [{0, Max}],
	Sizes = [{0, 0}],
	Ring2 = Ring?RYNG_RING{bits=Bits2, hasher=Hasher, max=Max,
		incrs=Incrs, sizes=Sizes, nodes=Nodes, ptrs=Ptrs},
	Reply = gen_server:call(?SERVER, {add_ring, Ring2}),
	{Reply, Ring2}.

%%%===================================================================
%%% Node API functions
%%%===================================================================

list_nodes(RingId) ->
	ryng_ring:list_nodes(RingId).

add_node(RingId, NodeObject) ->
	add_node(RingId, NodeObject, 0).

add_node(RingId, NodeObject, NodeWeight) ->
	add_node(RingId, NodeObject, NodeWeight, 0).

add_node(RingId, NodeObject, NodeWeight, NodePriority) ->
	ryng_ring:add_node(RingId, NodeObject, NodeWeight, NodePriority).

del_node(RingId, NodeObject) ->
	ryng_ring:del_node(RingId, NodeObject).

get_node(RingId, NodeObject) ->
	ryng_ring:get_node(RingId, NodeObject).

is_node(RingId, NodeObject) ->
	ryng_ring:is_node(RingId, NodeObject).

set_node(RingId, NodeObject, NodeWeight, NodePriority) ->
	ryng_ring:set_node(RingId, NodeObject, NodeWeight, NodePriority).

%%%===================================================================
%%% Object API functions
%%%===================================================================

balance_check(RingId, Iterations) ->
	case node_for(RingId, 0) of
		{ok, _} ->
			Start = erlang:now(),
			Balance = balance_check(RingId, Iterations, orddict:new()),
			Stop = erlang:now(),
			BalanceAverage = [{NodeObject, Count, Count / Iterations} || {NodeObject, Count} <- Balance],
			TimeTotal = timer:now_diff(Stop, Start),
			TimeAverage = TimeTotal / Iterations,
			{ok, {Iterations, TimeTotal, TimeAverage}, BalanceAverage};
		NodeError ->
			NodeError
	end.

balance_summary(RingId) ->
	case get_ring(RingId) of
		{ok, ?RYNG_RING{id=RingId}} ->
			case list_nodes(RingId) of
				{ok, Nodes} ->
					Summary = balance_summary_nodes(Nodes, dict:new()),
					{ok, Summary};
				NodesError ->
					NodesError
			end;
		RingError ->
			RingError
	end.

balance_summary_nodes([], Balance) ->
	Summary = dict:fold(fun(Priority, {Size, WeightedObjects}, S) ->
		gb_sets:union(S, gb_sets:from_list([{Priority, Object, Weight / Size} || {Object, Weight} <- WeightedObjects]))
	end, gb_sets:new(), Balance),
	gb_sets:to_list(Summary);
balance_summary_nodes([?RYNG_NODE{object=Object, priority=Priority, weight=Weight} | Nodes], Balance) ->
	Balance2 = dict:update(Priority, fun({Size, WeightedObjects}) ->
		{Size + Weight + 1, [{Object, Weight + 1} | WeightedObjects]}
	end, {Weight + 1, [{Object, Weight + 1}]}, Balance),
	balance_summary_nodes(Nodes, Balance2).

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
		{keypos, ?RYNG_RING.id},
		{write_concurrency, false},
		{read_concurrency, true}
	]),
	Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} || [Ref, Pid] <- ets:match(?TAB, {{pid, '$1'}, '$2'})],
	% _ = [begin

	% end || ?RYNG_RING{nodes=Nodes, ptrs=Ptrs} <- ets:match_object(?TAB_RINGS, '_')],
	{ok, #state{monitors=Monitors}}.

%% @private
handle_call({register_name, Name, Pid}, _From, State=#state{monitors=Monitors}) ->
	case ets:insert_new(?TAB, {{pid, Name}, Pid}) of
		true ->
			MonitorRef = erlang:monitor(process, Pid),
			{reply, yes, State#state{monitors=[{{MonitorRef, Pid}, Name} | Monitors]}};
		false ->
			{reply, no, State}
	end;
handle_call({drop_ring, RingId}, _From, State) ->
	Reply = ets:delete(?TAB_RINGS, RingId),
	{reply, Reply, State};
handle_call({add_ring, Ring=?RYNG_RING{}}, _From, State) ->
	Reply = ets:insert_new(?TAB_RINGS, Ring),
	{reply, Reply, State};
handle_call({refresh_ring, Ring=?RYNG_RING{}}, _From, State) ->
	true = ets:insert(?TAB_RINGS, Ring),
	ryng_event:ring_refresh(Ring),
	{reply, true, State};
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
balance_check(_RingId, 0, Balance) ->
	Balance;
balance_check(RingId, Iteration, Balance) ->
	{ok, NodeObject} = node_for(RingId, Iteration),
	balance_check(RingId, Iteration - 1, orddict:update_counter(NodeObject, 1, Balance)).

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

%% @private
refresh_ring_by_priority([], Ring=?RYNG_RING{max=Max}, SizesDict) ->
	{Sizes, Increments} = dict:fold(fun
		(P, 0, {SS, II}) ->
			{gb_sets:add_element({P, 0}, SS), gb_sets:add_element({P, Max}, II)};
		(P, S, {SS, II}) ->
			{gb_sets:add_element({P, S}, SS), gb_sets:add_element({P, Max div S}, II)}
	end, {gb_sets:new(), gb_sets:new()}, SizesDict),
	Ring?RYNG_RING{sizes=gb_sets:to_list(Sizes), incrs=gb_sets:to_list(Increments)};
refresh_ring_by_priority([?RYNG_NODE{priority=Priority, weight=Weight} | Nodes], Ring, Sizes) ->
	refresh_ring_by_priority(Nodes, Ring, dict:update_counter(Priority, Weight + 1, Sizes)).

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

%% @private
is_ring_owner(RingId) ->
	Self = self(),
	try whereis_name(RingId) of
		Self ->
			true;
		_ ->
			false
	catch
		_:_ ->
			false
	end.
