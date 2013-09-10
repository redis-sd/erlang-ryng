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
-module(ryng_ring_sup).
-behaviour(supervisor).

-include("ryng.hrl").

%% API
-export([start_link/1, ring_sup_name/1, graceful_shutdown/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Ring=#ring{}) ->
	SupName = ring_sup_name(Ring),
	supervisor:start_link({local, SupName}, ?MODULE, Ring).

ring_sup_name(#ring{name=Name}) ->
	list_to_atom("ryng_" ++ atom_to_list(Name) ++ "_ring_sup").

%% @doc Gracefully shutdown the named ring.
graceful_shutdown(Name) ->
	case catch ryng_ring:graceful_shutdown(Name) of
		ok ->
			ok;
		_ ->
			forced_shutdown
	end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Ring=#ring{}) ->
	RingSpec = {ryng_ring,
		{ryng_ring, start_link, [Ring]},
		transient, 2000, worker, [ryng_ring]},
	%% five restarts in 60 seconds, then shutdown
	Restart = {one_for_all, 5, 60},
	{ok, {Restart, [RingSpec]}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
