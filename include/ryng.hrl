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
	fun((binary()) -> binary()).

-record(ring, {
	name = undefined :: undefined | atom(),
	hash = sha       :: atom() | function() | {function(), [any()]} | {module(), atom(), [any()]},
	bits = undefined :: undefined | integer(),

	hasher = undefined :: undefined | ring_hasher(),
	inc    = undefined :: undefined | integer(),
	max    = undefined :: undefined | integer(),
	size   = undefined :: undefined | integer()
}).

-record(node, {
	object = undefined :: undefined | term(),
	weight = undefined :: undefined | non_neg_integer()
}).

-record(pointer, {
	index  = undefined :: undefined | integer(),
	object = undefined :: undefined | term()
}).
