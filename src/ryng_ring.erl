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
-module(ryng_ring).
-behaviour(gen_server).

-include("ryng.hrl").

%% API
-export([start_link/1, graceful_shutdown/1]).
-export([list_nodes/1, add_node/4, del_node/2, is_node/2]).
-export([hash_for/2, index_for/2, key_for/2, key_of/1, node_for/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
	name     = undefined :: undefined | atom(),
	nodes    = undefined :: undefined | ets:tid() | atom(),
	pointers = undefined :: undefined | ets:tid() | atom(),
	dirty    = true      :: boolean(),
	iref     = undefined :: undefined | reference()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @private
start_link(Ring=#ring{name=Name}) ->
	gen_server:start_link({local, Name}, ?MODULE, Ring, []).

%% @doc Gracefully shutdown the ring.
graceful_shutdown(RingName) ->
	gen_server:call(RingName, graceful_shutdown).

list_nodes(RingName) ->
	case ryng:is_ring(RingName) of
		true ->
			{ok, ets:match_object(nodes_table(RingName), '_')};
		false ->
			{ok, ring_not_found}
	end.

add_node(RingName, NodeObject, NodeWeight, NodePriority)
		when is_integer(NodeWeight) andalso NodeWeight >= 0
		andalso is_integer(NodePriority) andalso NodePriority >= 0 ->
	case ryng:is_ring(RingName) of
		true ->
			gen_server:call(RingName, {add_node, NodeObject, NodeWeight, NodePriority});
		false ->
			{error, ring_not_found}
	end.

del_node(RingName, NodeObject) ->
	case ryng:is_ring(RingName) of
		true ->
			gen_server:call(RingName, {del_node, NodeObject});
		false ->
			{error, ring_not_found}
	end.

is_node(RingName, NodeObject) ->
	case ryng:is_ring(RingName) of
		true ->
			ets:member(nodes_table(RingName), NodeObject);
		false ->
			{error, ring_not_found}
	end.

hash_for(RingName, Binary) when is_binary(Binary) ->
	case ryng:get_ring(RingName) of
		{ok, #ring{hasher=Hasher}} ->
			{ok, Hasher(Binary)};
		RingError ->
			RingError
	end.

index_for(RingName, Binary) when is_binary(Binary) ->
	case ryng:get_ring(RingName) of
		{ok, #ring{hasher=Hasher, incrs=Incrs, sizes=Sizes}} ->
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

key_for(RingName, Object) ->
	index_for(RingName, key_of(Object)).

key_of(Object) ->
	erlang:term_to_binary(Object).

node_for(RingName, Object) ->
	case key_for(RingName, Object) of
		{ok, Key} ->
			PTab = pointers_table(RingName),
			try ets:lookup_element(PTab, Key, #pointer.object) of
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
									try ets:lookup_element(PTab, FirstKey, #pointer.object) of
										NodeObject ->
											{ok, NodeObject}
									catch
										Class:Reason ->
											{error, {Class, Reason}}
									end
							end;
						NextKey ->
							try ets:lookup_element(PTab, NextKey, #pointer.object) of
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
init(Ring=#ring{name=Name}) ->
	Nodes = nodes_table(Name),
	Pointers = pointers_table(Name),
	Nodes = ryng_ets:soft_new(Nodes, [
		named_table,
		protected,
		ordered_set,
		{keypos, #node.object},
		{write_concurrency, false},
		{read_concurrency, true}
	]),
	Pointers = ryng_ets:soft_new(Pointers, [
		named_table,
		protected,
		ordered_set,
		{keypos, #pointer.index},
		{write_concurrency, false},
		{read_concurrency, true}
	]),
	true = ryng:set_ring(Ring, self()),
	{ok, IRef} = timer:send_interval(1000, tick),
	State = #state{name=Name, nodes=Nodes, pointers=Pointers, iref=IRef},
	{ok, State}.

%% @private
handle_call({add_node, NodeObject, NodeWeight, NodePriority}, _From, State=#state{name=RingName, nodes=Nodes}) ->
	Node = #node{object=NodeObject, weight=NodeWeight, priority=NodePriority},
	case ets:insert_new(Nodes, Node) of
		true ->
			ryng_event:node_add(RingName, Node),
			{reply, ok, State#state{dirty=true}};
		false ->
			{reply, {error, node_already_exists}, State}
	end;
handle_call({del_node, NodeObject}, _From, State=#state{name=RingName, nodes=Nodes, pointers=Pointers}) ->
	case ?MODULE:is_node(RingName, NodeObject) of
		true ->
			true = ets:match_delete(Pointers, #node{object=NodeObject, _='_'}),
			true = ets:delete(Nodes, NodeObject),
			ryng_event:node_del(RingName, NodeObject),
			{reply, ok, State#state{dirty=true}};
		false ->
			{reply, {error, node_not_found}, State}
	end;
handle_call(sync_ring, _From, State=#state{iref=IRef}) ->
	catch timer:cancel(IRef),
	{noreply, State2} = rebalance(State#state{dirty=false, iref=undefined}),
	IRef2 = timer:send_interval(1000, tick),
	{reply, ok, State2#state{iref=IRef2}};
handle_call(graceful_shutdown, _From, State) ->
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'ETS-TRANSFER', Table, _Pid, []}, State=#state{nodes=Nodes, pointers=Pointers}) when Table =:= Nodes orelse Table =:= Pointers ->
	{noreply, State};
handle_info(tick, State=#state{dirty=false}) ->
	{noreply, State};
handle_info(tick, State=#state{dirty=true}) ->
	rebalance(State#state{dirty=false});
handle_info(Info, State) ->
	error_logger:error_msg(
		"** ~p ~p unhandled info in ~p/~p~n"
		"   Info was: ~p~n",
		[?MODULE, self(), handle_info, 2, Info]),
	{noreply, State}.

%% @private
terminate(normal, #state{name=RingName, nodes=NTab, pointers=PTab}) ->
	catch ets:delete(PTab),
	catch ets:delete(NTab),
	ryng_event:ring_del(RingName),
	ok;
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
nodes_table(RingName) ->
	list_to_atom("ryng_" ++ atom_to_list(RingName) ++ "_nodes").

%% @private
pointers_table(RingName) ->
	list_to_atom("ryng_" ++ atom_to_list(RingName) ++ "_pointers").

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
rebalance(State=#state{name=RingName, nodes=NTab, pointers=PTab}) ->
	case ets:match_object(NTab, '_') of
		[] ->
			true = ets:match_delete(PTab, '_'),
			{noreply, State};
		Nodes when is_list(Nodes) ->
			case ryng:refresh_ring(RingName) of
				{ok, #ring{name=RingName, max=Max, incrs=Incrs}} ->
					Pointers = make_pointers(Nodes, Max, Incrs, 0, []),
					true = ets:match_delete(PTab, '_'),
					true = ets:insert(PTab, Pointers),
					{noreply, State};
				{error, ring_not_found} ->
					{noreply, State}
			end
	end.

%% @private
make_pointers([], _Maximum, _Increments, _Index, Pointers) ->
	Pointers;
make_pointers([#node{object=NodeObject, priority=NodePriority, weight=NodeWeight} | Nodes], Maximum, Increments, Index, Pointers) ->
	{NodePriority, Increment} = lists:keyfind(NodePriority, 1, Increments),
	{Index2, Pointers2} = make_pointer(NodeWeight+1, NodeObject, NodePriority, Maximum, Increment, Index, Pointers),
	make_pointers(Nodes, Maximum, Increments, Index2, Pointers2).

%% @private
make_pointer(0, _Object, _Priority, _Maximum, _Increment, Index, Pointers) ->
	{Index, Pointers};
make_pointer(Weight, Object, Priority, Maximum, Increment, Index, Pointers) ->
	Pointer = #pointer{index={Priority, Index}, object=Object},
	make_pointer(Weight - 1, Object, Priority, Maximum, Increment, Index + Increment, [Pointer | Pointers]).
