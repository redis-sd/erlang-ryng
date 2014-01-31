%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  10 Sep 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(ryng_event).

-include("ryng.hrl").

-define(MANAGER, ryng_manager).

%% API
-export([manager/0, add_handler/2]).
-export([node_add/2, node_del/2, node_set/2, ring_add/1, ring_del/1, ring_refresh/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

manager() ->
	?MANAGER.

add_handler(Handler, Pid) ->
	gen_event:add_handler(manager(), Handler, Pid).

node_add(RingName, Node) ->
	notify({node, add, RingName, Node}).

node_del(RingName, NodeObject) ->
	notify({node, del, RingName, NodeObject}).

node_set(RingName, Node) ->
	notify({ring, set, RingName, Node}).

ring_add(Ring) ->
	notify({ring, add, Ring}).

ring_del(RingName) ->
	notify({ring, del, RingName}).

ring_refresh(Ring) ->
	notify({ring, refresh, Ring}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
notify(Message) ->
	gen_event:notify(manager(), Message).
