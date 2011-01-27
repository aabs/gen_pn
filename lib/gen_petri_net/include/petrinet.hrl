%%%-------------------------------------------------------------------
%%% File    : petrinet.hrl
%%% Author  : user <matthews.andrew@gmail.com>
%%% Description : 
%%% This is the public API for the gen_petri behaviour. It allows you to
%%% define your states, transitions, markings and callbacks, then load 
%%% them into a gen_petri for execution.
%%%
%%% Created : 23 Jan 2011 by user <matthews.andrew@gmail.com>
%%%-------------------------------------------------------------------
%-module(petrinet).

%% API
%-export([]).
%-compile(export_all).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% place is used to provide a description of a place in the graph
%% 
%% #place
%% {
%%   name = some_term,
%%   capacity = infinity
%%   inhibitor = false
%% }
%%--------------------------------------------------------------------
-record(place,{
	name::term(), 
	capacity = infinity::positive_int(), 
	inhibitor::bool()
	}).

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-record(transition,{
	name::term(),
	priority=1::int()
}).

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-record(arc,{
	type::in|out,   % context of what goes in from and to
	from::term(), 	% name of where it came from
	to::term(),		% name of where it goes to
	weight=1::int() % weight of the transition (how many tokens are given or taken if it fires)
}).

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-record(place_mapping,{
	name::term,
	token_count=0::positive_int()
	}).

%	Q: How should data be stored in the data structure of the petri net.
%	A1. Dicts for each place and transition +  metadata about each of the above.
%	A2. A tree of information about the elements in the net. e.g. top level are 
%		places and below them are their transitions. Likewise for transitions
%	A3. Just store the transitions or just the places and they store just what they need
%	A4. Store an adjacency table for the links i.e. rows == trans & cols == places
%		Q:4.1 How do you represent a matrix using Erlang?
%	
%	
%	
%	
%	
%	
-record(petri_net, {
	name::term(),
	
	}).
%%--------------------------------------------------------------------
%% Record: place_marking
%% Description: The marking for a given place within a given marking.
%%--------------------------------------------------------------------
-record(place_marking, {
	%% the name of the place (a string)
	place_name, 
	%% the number of tokens in the place, within a given marking.(a non-negative
	%int)
	token_count
	}).

%%--------------------------------------------------------------------
%% Record: marking
%% Description: A specific marking for a petri net.
%%--------------------------------------------------------------------
-record(marking,{
	%% the name of this marking (e.g. the ID used to get it
	%% back from storage (e.g. a userś ID or similar)
	marking_id, 
	%% is list of tuples 
	tokens :: [#place_marking{}]
	}).
%---------------[functions for managing markings]-----------------

%%--------------------------------------------------------------------
%% Function: new_marking
%% Description: sets up a new marking with place names and tokens supplied
%% in a list of tuples {Name, NumTokens::int()}
%%--------------------------------------------------------------------
new_marking([])->
	#marking{};
new_marking([H | T])->
	#marking{}.
%%--------------------------------------------------------------------
%% Function: add_place
%% Description: Adds the name and initial token count of a place to a
%% marking
%%--------------------------------------------------------------------
add_place(Place, #marking{} = Marking)->
	not_implemented.
%%--------------------------------------------------------------------
%% Function: add_tokens_to_place
%% Description: A helper function to allow the setup and control of a 
%% Marking.
%%--------------------------------------------------------------------
add_tokens_to_place(#marking{} = M, Tokens) when is_integer(Tokens)->
	not_implemented.
%%--------------------------------------------------------------------
%% Function: signal
%% Description: Places a single token in the named place in the 
%% marking. This is intended to allow the net to receive notification
%% about events.
%%--------------------------------------------------------------------
signal(PlaceName, #marking{}=M)->
	not_implemented.
%%--------------------------------------------------------------------
%% Function: is_signalled
%% Description: Determines whether a place is marking, and returns
%% how many tokens are at the marked place.
%% Returns: {signalled, false, 0} | {signalled, true, X} where X > 0 |
% {signalled, false, is_inhibitor}
%%--------------------------------------------------------------------
is_signalled(PlaceName, #marking{}=M)->
	not_implemented.

%%====================================================================
%% Internal functions
%%====================================================================
