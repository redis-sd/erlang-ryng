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
-module(ryng_sup).
-behaviour(supervisor).

-include("ryng.hrl").

%% API
-export([start_link/0, new_ring/1, rm_ring/1, delete_ring/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Create a new ring from proplist ring config `RingConfig'. The
%% public API for this functionality is {@link ryng:new_ring/1}.
new_ring(RingConfig) ->
	NewRing = ryng_config:list_to_ring(RingConfig),
	Spec = ring_sup_spec(NewRing),
	supervisor:start_child(?MODULE, Spec).

%% @doc Gracefully shutdown the named ring.
rm_ring(Name) ->
	case ryng_ring_sup:graceful_shutdown(Name) of
		ok ->
			ok;
		forced_shutdown ->
			delete_ring(Name)
	end.

%% @doc Forcefully shutdown the named ring.
delete_ring(Name) ->
	SupName = ring_sup_name(Name),
	case supervisor:terminate_child(?MODULE, SupName) of
		{error, not_found} ->
			ok;
		ok ->
			supervisor:delete_child(?MODULE, SupName);
		Error ->
			Error
	end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
	%% a list of ring configs
	Configs = case application:get_env(ryng, rings) of
		{ok, C} ->
			C;
		undefined ->
			[]
	end,
	Rings = [ryng_config:list_to_ring(L) || L <- Configs],
	RingSupSpecs = [ring_sup_spec(Ring) || Ring <- Rings],
	{ok, {{one_for_one, 5, 10}, [
		?CHILD(ryng, worker)
		| RingSupSpecs
	]}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
ring_sup_spec(Ring=#ring{name=Name}) ->
	SupName = ring_sup_name(Name),
	{SupName,
		{ryng_ring_sup, start_link, [Ring]},
		transient, 5000, supervisor, [ryng_ring_sup]}.

%% @private
ring_sup_name(Name) ->
	list_to_atom("ryng_" ++ atom_to_list(Name) ++ "_ring_sup").
