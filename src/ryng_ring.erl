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
-module(ryng_ring).
-behaviour(gen_server).

-include("ryng.hrl").

%% API
-export([start_link/1, graceful_shutdown/1]).
-export([list_nodes/1, add_node/4, del_node/2, get_node/2, is_node/2,
	is_empty/1, set_node/4]).
-export([hash_for/2, index_for/2, key_for/2, key_of/1, node_for/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @private
start_link(Ring=?RYNG_RING{id=RingId}) ->
	gen_server:start_link({via, ryng, RingId}, ?MODULE, Ring, []).

%% @doc Gracefully shutdown the ring.
graceful_shutdown(RingId) ->
	gen_server:call({via, ryng, RingId}, graceful_shutdown).

list_nodes(RingId) ->
	case ryng:is_ring(RingId) of
		true ->
			{ok, ets:match_object(nodes_table(RingId), '_')};
		false ->
			{ok, ring_not_found}
	end.

add_node(RingId, NodeObject, NodeWeight, NodePriority)
		when is_integer(NodeWeight) andalso NodeWeight >= 0
		andalso is_integer(NodePriority) andalso NodePriority >= 0 ->
	case ryng:is_ring(RingId) of
		true ->
			gen_server:call({via, ryng, RingId}, {add_node, NodeObject, NodeWeight, NodePriority});
		false ->
			{error, ring_not_found}
	end.

del_node(RingId, NodeObject) ->
	case ryng:is_ring(RingId) of
		true ->
			gen_server:call({via, ryng, RingId}, {del_node, NodeObject});
		false ->
			{error, ring_not_found}
	end.

get_node(RingId, NodeObject) ->
	case ryng:is_ring(RingId) of
		true ->
			case ets:lookup(nodes_table(RingId), NodeObject) of
				[Node=?RYNG_NODE{}] ->
					{ok, Node};
				_ ->
					{error, node_not_found}
			end;
		false ->
			{error, ring_not_found}
	end.

is_node(RingId, NodeObject) ->
	case ryng:is_ring(RingId) of
		true ->
			ets:member(nodes_table(RingId), NodeObject);
		false ->
			{error, ring_not_found}
	end.

is_empty(RingId) ->
	case ryng:is_ring(RingId) of
		true ->
			case ets:first(nodes_table(RingId)) of
				'$end_of_table' ->
					true;
				_ ->
					false
			end;
		false ->
			{error, ring_not_found}
	end.

set_node(RingId, NodeObject, NodeWeight, NodePriority)
		when is_integer(NodeWeight) andalso NodeWeight >= 0
		andalso is_integer(NodePriority) andalso NodePriority >= 0 ->
	case ryng:is_ring(RingId) of
		true ->
			gen_server:call({via, ryng, RingId}, {set_node, NodeObject, NodeWeight, NodePriority});
		false ->
			{error, ring_not_found}
	end.

hash_for(RingId, Binary) when is_binary(Binary) ->
	case ryng:get_ring(RingId) of
		{ok, ?RYNG_RING{hasher=Hasher}} ->
			{ok, Hasher(Binary)};
		RingError ->
			RingError
	end.

index_for(RingId, Binary) when is_binary(Binary) ->
	case ryng:get_ring(RingId) of
		{ok, ?RYNG_RING{hasher=Hasher, incrs=Incrs, sizes=Sizes}} ->
			case non_empty_priority(Incrs, Sizes) of
				{ok, {Priority, Inc}, {Priority, Size}} ->
					Hash = Hasher(Binary),
					IntegerKey = case Hash of
						_ when is_binary(Hash) ->
							ryng:bin_to_int(Hash);
						_ ->
							Hash
					end,
					{ok, {Priority, (((IntegerKey div Inc) + 1) rem Size) * Inc}};
				PriorityError ->
					PriorityError
			end;
		RingError ->
			RingError
	end.

key_for(RingId, Object) ->
	index_for(RingId, key_of(Object)).

key_of(Object) ->
	erlang:term_to_binary(Object).

node_for(RingId, Object) ->
	case key_for(RingId, Object) of
		{ok, Key} ->
			PTab = ptrs_table(RingId),
			try ets:lookup_element(PTab, Key, ?RYNG_PTR.object) of
				NodeObject ->
					{ok, NodeObject}
			catch
				_:_ ->
					case ets:next(PTab, Key) of
						'$end_of_table' ->
							case ets:first(PTab) of
								'$end_of_table' ->
									{error, ring_empty};
								FirstKey ->
									try ets:lookup_element(PTab, FirstKey, ?RYNG_PTR.object) of
										NodeObject ->
											{ok, NodeObject}
									catch
										Class:Reason ->
											{error, {Class, Reason}}
									end
							end;
						NextKey ->
							try ets:lookup_element(PTab, NextKey, ?RYNG_PTR.object) of
								NodeObject ->
									{ok, NodeObject}
							catch
								Class:Reason ->
									{error, {Class, Reason}}
							end
					end
			end;
		KeyError ->
			KeyError
	end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Ring=?RYNG_RING{}) ->
	{true, Ring2} = ryng:resolve_ring(Ring),
	{ok, IRef} = timer:send_interval(1000, tick),
	Ring3 = Ring2?RYNG_RING{iref=IRef},
	{ok, Ring3}.

%% @private
handle_call({add_node, NodeObject, NodeWeight, NodePriority}, _From, Ring=?RYNG_RING{id=RingId, nodes=Nodes}) ->
	Node = ?RYNG_NODE{object=NodeObject, weight=NodeWeight, priority=NodePriority},
	case ets:insert_new(Nodes, Node) of
		true ->
			ryng_event:node_add(RingId, Node),
			{reply, ok, Ring?RYNG_RING{dirty=true}};
		false ->
			{reply, {error, node_already_exists}, Ring}
	end;
handle_call({del_node, NodeObject}, _From, Ring=?RYNG_RING{id=RingId, nodes=Nodes, ptrs=Ptrs}) ->
	case ?MODULE:is_node(RingId, NodeObject) of
		true ->
			true = ets:match_delete(Ptrs, ?RYNG_NODE{object=NodeObject, _='_'}),
			true = ets:delete(Nodes, NodeObject),
			ryng_event:node_del(RingId, NodeObject),
			{reply, ok, Ring?RYNG_RING{dirty=true}};
		false ->
			{reply, {error, node_not_found}, Ring}
	end;
handle_call({set_node, NodeObject, NodeWeight, NodePriority}, _From, Ring=?RYNG_RING{id=RingId, nodes=Nodes}) ->
	Node = ?RYNG_NODE{object=NodeObject, weight=NodeWeight, priority=NodePriority},
	case ?MODULE:get_node(RingId, NodeObject) of
		{ok, Node} ->
			{reply, ok, Ring};
		{ok, _} ->
			true = ets:insert(Nodes, Node),
			ryng_event:node_set(RingId, Node),
			{reply, ok, Ring?RYNG_RING{dirty=true}};
		{error, node_not_found} ->
			true = ets:insert(Nodes, Node),
			ryng_event:node_add(RingId, Node),
			{reply, ok, Ring?RYNG_RING{dirty=true}};
		RingError ->
			{reply, RingError, Ring}
	end;
handle_call(sync_ring, _From, Ring=?RYNG_RING{iref=IRef}) ->
	catch timer:cancel(IRef),
	{noreply, Ring2} = rebalance(Ring?RYNG_RING{dirty=false, iref=undefined}),
	IRef2 = timer:send_interval(1000, tick),
	{reply, ok, Ring2?RYNG_RING{iref=IRef2}};
handle_call(graceful_shutdown, _From, Ring) ->
	{stop, normal, ok, Ring};
handle_call(_Request, _From, Ring) ->
	{reply, ignore, Ring}.

%% @private
handle_cast(_Request, Ring) ->
	{noreply, Ring}.

%% @private
handle_info({'ETS-TRANSFER', Table, _Pid, []}, Ring=?RYNG_RING{nodes=Nodes, ptrs=Ptrs}) when Table =:= Nodes orelse Table =:= Ptrs ->
	{noreply, Ring};
handle_info(tick, Ring=?RYNG_RING{dirty=false}) ->
	{noreply, Ring};
handle_info(tick, Ring=?RYNG_RING{dirty=true}) ->
	rebalance(Ring?RYNG_RING{dirty=false});
handle_info(Info, Ring=?RYNG_RING{id=RingId}) ->
	error_logger:error_msg(
		"** ~p ~p unhandled info in ~p/~p~n"
		"   Info was: ~p~n",
		[?MODULE, RingId, handle_info, 2, Info]),
	{noreply, Ring}.

%% @private
terminate(normal, ?RYNG_RING{id=RingId, nodes=NTab, ptrs=PTab}) ->
	catch ets:delete(NTab),
	catch ets:delete(PTab),
	ryng_event:ring_del(RingId),
	ok;
terminate(_Reason, _Ring) ->
	ok.

%% @private
code_change(_OldVsn, Ring, _Extra) ->
	{ok, Ring}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
nodes_table(RingId) ->
	ryng:nodes_table(RingId).

%% @private
ptrs_table(RingId) ->
	ryng:ptrs_table(RingId).

%% @private
non_empty_priority([], _) ->
	{error, ring_empty};
non_empty_priority(_, []) ->
	{error, ring_empty};
non_empty_priority([{Priority, _} | Increments], [{Priority, 0} | Sizes]) ->
	non_empty_priority(Increments, Sizes);
non_empty_priority([{Priority, Increment} | _], [{Priority, Size} | _]) ->
	{ok, {Priority, Increment}, {Priority, Size}}.

%% @private
rebalance(Ring=?RYNG_RING{id=RingId, nodes=NTab, ptrs=PTab}) ->
	case ets:match_object(NTab, '_') of
		[] ->
			true = ets:match_delete(PTab, '_'),
			{noreply, Ring};
		Nodes when is_list(Nodes) ->
			case ryng:refresh_ring(RingId) of
				{true, ?RYNG_RING{id=RingId, max=Max, incrs=Incrs}} ->
					Pointers = make_pointers(Nodes, Max, Incrs, 0, []),
					true = ets:match_delete(PTab, '_'),
					true = ets:insert(PTab, Pointers),
					{noreply, Ring};
				{error, ring_not_found} ->
					{noreply, Ring}
			end
	end.

%% @private
make_pointers([], _Maximum, _Increments, _Index, Pointers) ->
	Pointers;
make_pointers([?RYNG_NODE{object=NodeObject, priority=NodePriority, weight=NodeWeight} | Nodes], Maximum, Increments, Index, Pointers) ->
	{NodePriority, Increment} = lists:keyfind(NodePriority, 1, Increments),
	{Index2, Pointers2} = make_pointer(NodeWeight+1, NodeObject, NodePriority, Maximum, Increment, Index, Pointers),
	make_pointers(Nodes, Maximum, Increments, Index2, Pointers2).

%% @private
make_pointer(0, _Object, _Priority, _Maximum, _Increment, Index, Pointers) ->
	{Index, Pointers};
make_pointer(Weight, Object, Priority, Maximum, Increment, Index, Pointers) ->
	Pointer = ?RYNG_PTR{index={Priority, Index}, object=Object},
	make_pointer(Weight - 1, Object, Priority, Maximum, Increment, Index + Increment, [Pointer | Pointers]).
