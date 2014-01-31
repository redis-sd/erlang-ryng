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
new_ring(RingConfig) when is_list(RingConfig) ->
	new_ring(ryng_config:list_to_ring(RingConfig));
new_ring(Ring=?RYNG_RING{}) ->
	Spec = ring_spec(Ring),
	supervisor:start_child(?MODULE, Spec).

%% @doc Gracefully shutdown the named ring.
rm_ring(Id) ->
	case ryng_ring:graceful_shutdown(Id) of
		ok ->
			ok;
		forced_shutdown ->
			delete_ring(Id)
	end.

%% @doc Forcefully shutdown the named ring.
delete_ring(Id) ->
	case supervisor:terminate_child(?MODULE, {ryng_ring, Id}) of
		{error, not_found} ->
			ok;
		ok ->
			supervisor:delete_child(?MODULE, {ryng_ring, Id});
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
	RingSpecs = [ring_spec(Ring) || Ring <- Rings],
	%% five restarts in 10 seconds, then shutdown
	Restart = {one_for_all, 5, 10},
	{ok, {Restart, [
		?CHILD(ryng, worker)
		| RingSpecs
	]}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
ring_spec(Ring=?RYNG_RING{id=Id}) ->
	{{ryng_ring, Id},
		{ryng_ring, start_link, [Ring]},
		transient, 2000, worker, [ryng_ring]}.
