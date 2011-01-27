-module(gen_petri_net).

%% API

-export([start_link/4]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
	[{on_fire, 1}];
behaviour_info(_Other) ->
	undefined.

start_link(_States, _Transitions, _Arcs, _StartStates) ->
	proc_lib:start_link(?MODULE, link, loop, start(_States, _Transitions, _Arcs, _StartStates), []).

loop(State) ->
    Msg = receive
	      Input ->
		    Input
	  after Time ->
		  {'$gen_event', timeout}
	  end,
	  dispatch_event(Input, State).
	  
start(States, Transitions, Arcs, StartStates) ->
	construct_petri_net(States, Transitions, Arcs, StartStates).

%% Function: dispatch_event
%% Description: this works out what the input is that is coming in, and how it maps to tokens being put into
%% places. After that it proceeds to fire transitions in priority order till there are either no more enabled
%% transitions or until the limit of fires is reached.

dispatch_event({continue, Count}, State) -> % continue firing enabled transitions until Count expires
	loop(State);
dispatch({signal, PlaceName, Count}, State)->	% set a token in the input place, and proceed to fire until Count expires
	loop(State);
dispatch_event(Input, State) ->
	report_error(¨unknown input ~p~n¨, [Input]),
	loop(State).
	
report_error(Msg, Args)->
	io:format(Msg, Args).	
%% outcomes of firing:
%% {ok, [t1, t3]} - success, with a list of the transitions that were fired
%% {ok, []} 		success, but no events were fired
%% {error, undefined, [SomeDetails]} - the incoming message was not understood
%% {error, undefined, [SomeDetails]} - the incoming message was not understood

-record(petri_net, {
	places
	}).
-record(marking, {
	marking_id, 	% some user supplied identifier (e.g. user Id)
	tokens			% [{place_name :: atom(), token_count :: int()}]
	}).
-record(place, {place_id, capacity::int(), inhibit :: bool()}).

construct_petri_net(States, Transitions, Arcs, StartStates) ->
	ValidationResult = CheckModel(States, Transitions, Arcs, StartStates),
	case ValidationResult of
		ok ->
			#petri_net{
				places = States
			};
		_ ->
			{error, invalid_model}
	end.

% go through the collection of in and out specs, extracting out a unique set of transitions
% then construct for each of them two lists: in and out. ´in´ contains tuples of the form {state_name, weight, and inhib}
% out contains tuples of the form {state_name, weight}
construct_adjacency_matrix([], Acc)->
	Acc;
construct_adjacency_matrix(Arcs)->
	lists:foldl(fun(Elem, AccIn) -> case element(1, Elem) of
			´in´ ->
				sets:add_element(element(3,Elem), AccIn);
			´out´ ->
				sets:add_element(element(2,Elem), AccIn) 
			end
		end,
		sets:new(),
		Arcs) ,
	
	NewAcc = dict:append(),
	
