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

-type ring_hasher() ::
	fun((binary()) -> bitstring() | integer()).

-type node_object()   :: term().
-type node_priority() :: non_neg_integer().
-type node_weight()   :: non_neg_integer().

-type pointer_index() :: non_neg_integer().

-record(ring, {
	name = undefined :: undefined | atom(),
	hash = sha       :: atom() | function() | {function(), [any()]} | {module(), atom(), [any()]},
	bits = undefined :: undefined | non_neg_integer(),

	hasher = undefined :: undefined | ring_hasher(),
	max    = undefined :: undefined | pointer_index(),
	incrs  = undefined :: undefined | [{node_priority(), pointer_index()}],
	sizes  = undefined :: undefined | [{node_priority(), node_weight()}]
}).

-record(node, {
	object   = undefined :: undefined | node_object(),
	priority = undefined :: undefined | node_priority(),
	weight   = undefined :: undefined | node_weight()
}).

-record(pointer, {
	index  = undefined :: undefined | {node_priority(), pointer_index()},
	object = undefined :: undefined | node_object()
}).
