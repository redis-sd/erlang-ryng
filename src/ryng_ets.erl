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
-module(ryng_ets).
-behaviour(gen_server).

-include("ryng.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, new/2, soft_new/2, give_away/1, is_table/1, watch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
	orphans = dict:new() :: dict()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @private
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec new(atom() | {atom(), reference()}, [proplists:property()])
	-> ets:tid() | atom().
new(Name, Options) when is_atom(Name) ->
	case lists:member(named_table, Options) of
		true ->
			new({Name, erlang:make_ref()}, Options);
		false ->
			erlang:error(badarg, [Name, Options])
	end;
new(Name, Options) when is_tuple(Name) ->
	case gen_server:call(?SERVER, {new, [Name, Options]}) of
		{caught, Class, Reason} ->
			erlang:Class(Reason);
		Reply ->
			Reply
	end.

-spec soft_new(atom(), [proplists:property()])
	-> ets:tid() | atom().
soft_new(Name, Options) when is_atom(Name) ->
	case is_table(Name) of
		true ->
			case ?MODULE:give_away(Name) of
				Name ->
					Name;
				not_found ->
					true = ?MODULE:watch(Name),
					Name
			end;
		false ->
			?MODULE:new(Name, Options)
	end.

%% @doc Returns ownership of an orphaned ets table to calling process.
-spec give_away(Name::atom())
	-> ets:tid() | not_found.
give_away(Tab) ->
	gen_server:call(?SERVER, {give_away, Tab}).

-spec is_table(Tab::atom())
	-> boolean().
is_table(undefined) ->
	false;
is_table(Tab) when is_atom(Tab) ->
	Tab == ets:info(Tab, name).

-spec watch(Tab::atom())
	-> true.
watch(Tab) when is_atom(Tab) ->
	ets:setopts(Tab, {heir, whereis(?SERVER), Tab}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
	erlang:process_flag(trap_exit, true),
	State = #state{},
	{ok, State}.

%% @private
handle_call({new, [{Name, Ref}, DirtyOptions]}, {Owner, _}, State) ->
	try
		Options = clean_options(DirtyOptions),
		HeirData = case lists:member(named_table, Options) of
			true ->
				Name;
			false ->
				{Name, Ref}
		end,
		Tab = ets:new(Name, [{heir, self(), HeirData} | Options]),
		ets:give_away(Tab, Owner, []),
		{reply, Tab, State}
	catch
		Class:Pattern ->
			{reply, {caught, Class, Pattern}, State}
	end;
handle_call({give_away, Name}, {Owner, _}, State) ->
	case remove_orphan(Name, State) of
		not_found ->
			{reply, not_found, State};
		{Tab, State2} ->
			ets:give_away(Tab, Owner, []),
			{reply, Tab, State2}
	end;
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'EXIT', _Pid, _Reason}, State) ->
	{noreply, State};
handle_info({'ETS-TRANSFER', Tab, _FromPid, HeirData}, State) ->
	{noreply, add_orphan(HeirData, Tab, State)};
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

%% @doc Strip out any existing heir data.
%% @private
clean_options(DirtyOptions) ->
	lists:dropwhile(fun
		(T) when is_tuple(T) andalso element(1, T) =:= heir ->
			true;
		(_) ->
			false
	end, DirtyOptions).

%% @private
add_orphan(Key, Tab, State) ->
	State#state{orphans=dict:store(Key, Tab, State#state.orphans)}.

%% @private
remove_orphan(Key, State=#state{orphans=Orphans}) ->
	case dict:find(Key, Orphans) of
		{ok, Tab} ->
			{Tab, State#state{orphans=dict:erase(Key, Orphans)}};
		error ->
			not_found
	end.
