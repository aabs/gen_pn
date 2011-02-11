%%% -------------------------------------------------------------------
%%% Author  : aabs
%%% Description :
%%%
%%% Created : 09/02/2011
%%% -------------------------------------------------------------------
-module(sample_pn).

-behaviour(gen_pn).
-include("petrinet.hrl").
-export([start/0, start/1]).
-export([init/3, terminate/3, code_change/4]).

% transition functions
-export([t1/5]). 

-record(state, {previous_states = []}).

start()->
	start(undefined).
start(Name)->
	Args =[
		   {places,[
			  #place{name=p1, initial_marking=0},
			  #place{name=p2, initial_marking=1},
			  #place{name=p3, initial_marking=1},
			  #place{name=p4, initial_marking=0}
			]},
		   {transitions, [#transition{name=t1}]},
		   {name, Name},
		   {arcs,[{t1, in,  [#arc{place = p1},#arc{place = p2},#arc{place = p3}]}, 
				  {t1, out, [#arc{place = p4}]}]}		   
		   ],
	gen_pn:start({local, Name}, ?MODULE, Args, []).

init(_Args, _Marking, _PN) ->
    {ok, #state{}}.

t1(StateData, PN, M, M2, Args) ->
	io:format("invoked t1"),
    {ok, hello, StateData#state{previous_states = [M] ++ StateData#state.previous_states}}.

terminate(_Reason, _StateName, _StatData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
