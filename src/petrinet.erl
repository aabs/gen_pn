%% @author Andrew Matthews <matthews.andrew@gmail.com>
%% Created: 01/02/2011
%% @doc A simple petri net model. This model is primarily intended for use withing the gen_pn module. It can however be used
%% independently, in much the same way as gen_pn uses it.
%% 
%% The petrinet does not keep track of user code to be invoked in the event of a transition firing. Instead it provides
%% user code with information about what transition would fire next under a given graph/marking combination. The user code,
%% such as gen_pn, can then determine how and when the user code was to be invoked.
%% ===Usage===
%% Construct the fully-formed petri net like so:
%% ```	Places = [
%% 			  #place{name=p1, initial_marking=1},
%% 			  #place{name=p2, initial_marking=1},
%% 			  #place{name=p3, initial_marking=1},
%% 			  #place{name=p4, initial_marking=0}],
%% 	Transitions = [#transition{name=t1}],
%% 	Name = blah,
%% 	{PN, M} = petrinet:new(Name, 
%% 	                    Places, 
%% 	                    Transitions, 
%% 	                       [{t1, in, [
%% 	                           #arc{place = p1},
%% 	                           #arc{place = p2},
%% 	                           #arc{place = p3}]},
%% 	                        {t1, out, [
%% 	                           #arc{place = p4}]}])'''
%% 
%% The net is ready for immediate use:
%% 	```FireList = petrinet:get_fire_list(PN3, M),
%% 	?assertMatch([{t1, [{p1, 0}, {p2, 0}, {p3, 0}, {p4, 1}]}], FireList).'''
%% this demonstrates that `get_fire_list' works out what the enabled transition `T' with the highest priority would be
%% it then calculates what the marking would be transformed into if `T' fired. It is then up to the user
%% code to dtermine whether to accept the transformed Marking. The issue is that if user code throws and error, the PN
%% is perfectly able to return to the original marking before `T' fired.


-module(petrinet).
-include("petrinet.hrl").

-export([new/1,new/3,new/4,add_places/2,add_arc/4,add_arcs/4,add_transitions/2]).
-export([get_fire_list/2, set_signal/3, signal/2]).


-export([new/1,new/3,new/4,add_places/2,add_arc/4,add_arcs/4,add_transitions/2,enabled_transitions/2]).
-export([get_arcs/3,get_markings_for_transition/3,inarc_exists/2,new_marking/1,outarc_exists/2,transition_is_enabled/3]).
-export([calculate_effect_on_marking/3,adjust_marking/3,get_next_transition_to_fire/2,get_fire_list/2,get_fire_list/3,add_arcs/2, set_signal/3, signal/2]).

%-compile(export_all).

%% @doc Construct a new (empty) petri net. Use this when you wish to iteratively add places transitions and arcs after the net is constructed.
%% @spec new(Name::atom())->#petri_net{}
-spec new(Name::atom())->#petri_net{}.
new(Name)->
	M = new_marking([]),
	{#petri_net{name = Name}, M}.

%% @doc Construct a new Petri Net with supplied places and transitions (but no arcs).
%% Places is a lists of `#place{}' and Transitions is a list of `#transition{}'.
%% Construct the disconnected petri net like so:
%% ```new2_test()-> 
%% 		petrinet:new{
%%			name=my_petri_net, 
%%			places = [#place{name=p1},#place{name=p2},#place{name=p3}], 
%%			transitions = [#transition{name=t1}.'''
%%
%% The result is a tuple with the constructed petri net, plus a marking based on the places supplied.
%% @spec new(Name::atom(), Places::list(#place{}), Transitions::list(#transition{})) -> #petri_net{}
-spec new(Name::atom(), Places::list(#place{}), Transitions::list(#transition{})) -> #petri_net{}.
new(Name, Places, Transitions) ->
	Adjss = lists:foldl(fun(Elem, AccIn)-> 
		[{Elem#transition.name, adj_list:new(Elem#transition.name)} | AccIn] end, [], Transitions),
	PN = #petri_net{ name = Name,
					 places=Places, 
					 transitions=Transitions,
					 inarcs = Adjss,
					 outarcs = Adjss
					},
	{ PN, new_marking(Places) }.

%% @doc Construct a new Petri Net with supplied places and transitions and arcs.
%% Places is a lists of `#place{}' and Transitions is a list of `#transition{}', Arcs is a list .
%% Construct the fully-formed petri net like so:
%% ```	Places = [
%% 			  #place{name=p1, initial_marking=1},
%% 			  #place{name=p2, initial_marking=1},
%% 			  #place{name=p3, initial_marking=1},
%% 			  #place{name=p4, initial_marking=0}],
%% 	Transitions = [#transition{name=t1}],
%% 	Name = blah,
%% 	{PN, M} = petrinet:new(Name, 
%% 	                    Places, 
%% 	                    Transitions, 
%% 	                       [{t1, in, [
%% 	                           #arc{place = p1},
%% 	                           #arc{place = p2},
%% 	                           #arc{place = p3}]},
%% 	                        {t1, out, [
%% 	                           #arc{place = p4}]}])'''
%% 
%% The net is ready for immediate use.
%% 
%% @spec new(Name::atom(), 
%% 		Places::list(#place{}), 
%% 		Transitions::list(#transition{}), 
%% 		Arcs::[{TranLabel::atom(), Type::in|out, Arcs::list(#arc{})}]) 
%% 		-> #petri_net{}
-spec new(Name::atom(), Places::list(#place{}), Transitions::list(#transition{}), Arcs::[{TranLabel::atom(), Type::in|out, Arcs::list(#arc{})}]) -> #petri_net{}.
new(Name, Places, Transitions, Arcs) ->
	{PN, M} = new(Name, Places, Transitions),
	PN2 = add_arcs(PN, Arcs),
	{ PN2, M}.

%% @doc add a list of places to an existing petri net
%% @spec add_places(Places::list(#place{}), PN::#petri_net{}) -> #petri_net{}
-spec add_places(Places::list(#place{}), PN::#petri_net{}) -> #petri_net{}.
add_places([], PN) ->
	PN;
add_places(Places, PN) when is_list(Places) ->
	X1 = sets:from_list(PN#petri_net.places),
	X2 = sets:from_list(Places),
	X3 = sets:union(X1, X2),
	PN#petri_net{places = sets:to_list(X3)}.

%% @doc create an arc of type Type (in or out) between the transition with label TransitionLabel
%% and the place labelled PlaceLabel. Use defaults for all other settings
%% @spec add_arc(PetriNet::#petri_net{}, TransitionLabel::atom(), Arc::#arc{}, Type :: in|out)-> #petri_net{}
-spec add_arc(PetriNet::#petri_net{}, TransitionLabel::atom(), Arc::#arc{}, Type :: in|out)-> #petri_net{}.
add_arc(PN, TransitionLabel, Arc, Type)->
	case Type of
		in ->
			{TransitionLabel, AdjList} = lists:keyfind(TransitionLabel, 1, PN#petri_net.inarcs),
			NewAdjList = adj_list:add_adj(Arc, AdjList),
			PN#petri_net{inarcs = lists:keyreplace(TransitionLabel, 1, PN#petri_net.inarcs, {TransitionLabel, NewAdjList})};
		out ->
			{TransitionLabel, AdjList} = lists:keyfind(TransitionLabel, 1, PN#petri_net.outarcs),
			NewAdjList = adj_list:add_adj(Arc, AdjList),
			PN#petri_net{outarcs = lists:keyreplace(TransitionLabel, 1, PN#petri_net.outarcs, {TransitionLabel, NewAdjList})}
	end.


%% @spec add_arcs(PN::#petri_net{}, Arcs::[{TranLabel::atom(), Type::in|out, Arcs::list(#arc{})}]) -> #petri_net{}
-spec add_arcs(PN::#petri_net{}, Arcs::[{TranLabel::atom(), Type::in|out, Arcs::list(#arc{})}]) -> #petri_net{}.
add_arcs(PN, [])->PN;
add_arcs(PN, Arcs)->
  lists:foldl(fun({TranLabel, Type, Arcs}, PNIn) -> add_arcs(PNIn, TranLabel, Arcs, Type) end, 
	PN, Arcs).


%% @doc add a list of arcs to an existing petri net
%% @spec add_arcs(PN::#petri_net{}, TransitionLabel::atom(), Arcs::[#arc{}], Type::in|out)-> #petri_net{}
-spec add_arcs(PN::#petri_net{}, TransitionLabel::atom(), Arcs::[#arc{}], Type::in|out)-> #petri_net{}.
add_arcs(PN, _TransitionLabel, [], _Type)->
	PN;
add_arcs(PN, TransitionLabel, Arcs, Type)->
	lists:foldl(fun(Elem, AccIn)-> add_arc(AccIn, TransitionLabel, Elem, Type) end, PN, Arcs).

%% @doc get arcs into or out of a given transition of a petri net
%% @spec get_arcs(PN::#petri_net{}, TransitionLabel::atom(), Type :: in|out) -> list({atom(), #arc{}})
-spec get_arcs(PN::#petri_net{}, TransitionLabel::atom(), Type :: in|out) -> list({atom(), #arc{}}).
get_arcs(PN, TransitionLabel, Type)->
	AdjList = case Type of
				  in ->  PN#petri_net.inarcs;
				  out -> PN#petri_net.outarcs
			  end,
	{TransitionLabel, X} = lists:keyfind(TransitionLabel, 1, AdjList),
	adj_list:get_adjs(X).

%% @doc add a list of transitions to an existing petri net
%% @spec add_transitions(Transitions::list(#transition{}), PN::#petri_net{})-> #petri_net{}
-spec add_transitions(Transitions::list(#transition{}), PN::#petri_net{})-> #petri_net{}.
add_transitions([], PN) ->
	PN;
add_transitions(Transitions, PN) when is_list(Transitions) ->
	% create an entry for any adjacency lists that were not already present
	PN2 = lists:foldl(
			fun(#transition{name = Tran}, AccPn)->
				case inarc_exists(Tran, PN) of
					false->
						AccPn#petri_net{inarcs = [{Tran, adj_list:new(Tran)}] ++ AccPn#petri_net.inarcs};
					_ ->	
						AccPn
				end
			end, 
			PN, Transitions),
	PN3 = lists:foldl(
			fun(#transition{name = Tran}, AccPn)->
				case outarc_exists(Tran, PN) of
					false->
						AccPn#petri_net{outarcs = [{Tran, adj_list:new(Tran)}] ++ AccPn#petri_net.outarcs};
					_ ->	
						AccPn
				end
			end, 
			PN2, Transitions),	

	% add new transitions to the net
	X1 = sets:from_list(PN#petri_net.transitions),
	X2 = sets:from_list(Transitions),
	X3 = sets:union(X1, X2),
	PN3#petri_net{transitions = sets:to_list(X3)}.

%% @doc tests whether an entry exists for a given transition in the list of inarcs (list of adj_lists)
%% @spec inarc_exists(TranLabel::atom(), PN::#petri_net{})-> boolean()
-spec inarc_exists(TranLabel::atom(), PN::#petri_net{})-> boolean().
inarc_exists(TranLabel, PN)->
	case lists:keyfind(TranLabel, 2, PN#petri_net.inarcs) of
		false->
			false;
		_ ->
			true
	end.

%% @doc tests whether an entry exists for a given transition in the list of outarcs (list of adj_lists)
%% @spec outarc_exists(TranLabel::atom(), PN::#petri_net{})-> boolean()
-spec outarc_exists(TranLabel::atom(), PN::#petri_net{})-> boolean().
outarc_exists(TranLabel, PN)->
	case lists:keyfind(TranLabel, 2, PN#petri_net.outarcs) of
		false->
			false;
		_ ->
			true
	end.

%% @doc finds the incoming arcs into a transition and finds the associated places, 
%% their number of tokens, and whether they are inhibitor links or not
%% @spec get_markings_for_transition(TransitionLabel::atom(), PN::#petri_net{}, M::[{atom(),integer()}])-> 
%% 	[{atom(),
%% 	integer(),
%% 	integer(),
%% 	boolean(),
%% 	boolean()}]
-spec get_markings_for_transition(TransitionLabel::atom(), PN::#petri_net{}, M::[{atom(),integer()}])-> 
	list({atom(), %place label
	integer(), % tokens in place under marking M
	integer(), % weight of arc between place and transition
	boolean(), % whether the arc is an inhibitor or not
	boolean()}). % whether the arc qualifies to enable the transition
get_markings_for_transition(TransitionLabel, PN, M)->
	{TransitionLabel, AdjList} = lists:keyfind(TransitionLabel, 1, PN#petri_net.inarcs),
	Arcs = adj_list:get_adjs(AdjList),
	Results = lists:foldl(fun(#arc{inhibitor=Inh,place=Place,weight=Weight}, AccIn)-> 
						{Place, Tokens} = lists:keyfind(Place, 1, M),
						Qualifies = ((Inh and (Tokens == 0)) orelse ((not Inh) and (Tokens >= Weight))),
						[{Place, Tokens, Weight, Inh, Qualifies} | AccIn]
						end, [], Arcs),
	lists:reverse(Results).

%% @doc tests whether all arcs into a given transition qualify (i.e. are either from places 
%% that have more tokens than needed or have no tokens but are inhibitors)
%% @spec transition_is_enabled(T::atom(), PN::#petri_net{}, M::[{atom(),integer()}])-> boolean()
-spec transition_is_enabled(T::atom(), PN::#petri_net{}, M::[{atom(),integer()}])-> boolean().
transition_is_enabled(T, PN, M)->
	X = get_markings_for_transition(T, PN, M),
	lists:all(fun(Elem)-> element(5, Elem) end, X).

%% @doc retrieves the list of all transitions that are enabled on the petri net for the given marking
%% @spec enabled_transitions(PN::#petri_net{}, M::[{atom(),integer()}])-> list(#transition{})
-spec enabled_transitions(PN::#petri_net{}, M::[{atom(),integer()}])-> list(#transition{}).
enabled_transitions(PN, M)->
	lists:sort(fun(A,B)-> (A#transition.priority > B#transition.priority) end, 
		lists:filter(fun(T)->transition_is_enabled(T#transition.name, PN, M) end, PN#petri_net.transitions)). 

%% @doc constructs a new marking list
%% @spec new_marking(Places::list(#place{})) -> list({atom(),integer()})
-spec new_marking(Places::list(#place{})) -> list({atom(),integer()}).
new_marking([])->
	[];
new_marking(Places)->
	[{X#place.name, X#place.initial_marking} || X <- Places].

signal(PlaceLabel, M)-> set_signal(PlaceLabel, 1, M).

set_signal(PlaceLabel, Tokens, M)->
	lists:keyreplace(PlaceLabel, 1, M, {PlaceLabel, Tokens}).

%% @doc get the next transition to fire, and the resulting marking after firing has been performed.
%% @spec get_next_transition_to_fire(PN::#petri_net{}, M::[{atom(), integer()}]) -> {atom(), [{atom(), integer()}]} | non_live
-spec get_next_transition_to_fire(PN::#petri_net{}, M::[{atom(), integer()}]) -> {atom(), [{atom(), integer()}]} | non_live.
get_next_transition_to_fire(PN, M) -> 
	case enabled_transitions(PN, M) of 
		[] -> non_live;
		[#transition{name=Tran,priority=_P} | _T] ->
			{Tran, calculate_effect_on_marking(Tran, PN, M)}
	end.

%% @doc Calculate what the next marking will be if a transition is fired.
%%
%% <ul>
%% <li>1. foreach arc `a' going into the transition</li>
%% <li>if `!a.inhibitor' then</li>
%% <ul><li>M1 = M[a.name].tokens - M[a.name].weight</li></ul> 
%% <li>foreach arc 'b' leaving the transition</li>
%% <ul><li>M2 = M1[a.name].tokens + M1[a.name].weight</li></ul>
%% </ul>
%% @spec calculate_effect_on_marking(TransitionLabel::atom(), PN::#petri_net{}, M::[{atom(), integer()}]) ->[{atom(), integer()}]
-spec calculate_effect_on_marking(Tl::atom(), PN::#petri_net{}, M::[{atom(), integer()}]) ->[{atom(), integer()}].
calculate_effect_on_marking(Tl, PN, M) ->
	IAs = get_arcs(PN, Tl, in),
	OAs = get_arcs(PN, Tl, out),
	M2 = lists:foldl(fun(Elem, Mi)-> adjust_marking(Mi, Elem#arc.place, -1 * Elem#arc.weight) end, 
					 M, IAs),
	M3 = lists:foldl(fun(Elem, Mi)-> adjust_marking(Mi, Elem#arc.place, Elem#arc.weight) end, 
					 M2, OAs),
	M3.

%% @doc Adjust a marking for a given place with its old value plus some difference (Delta).
%% @spec adjust_marking(M::[{atom(), integer()}], Pl::atom(), Delta::integer())-> [{atom(), integer()}]
-spec adjust_marking(M::[{atom(), integer()}], Pl::atom(), Delta::integer())-> [{atom(), integer()}].
adjust_marking(M, Pl, Delta)->
	{Pl, TokenCount} = lists:keyfind(Pl, 1, M),
	lists:keyreplace(Pl, 1, M, {Pl, TokenCount + Delta}).

%% @doc Gets the next transition that will fire, plus the marking that will result after the transition fires.
%% @spec get_fire_list(PN::#petri_net{}, M::list({atom(), integer()}))-> [{atom(), list({atom(), integer()})}]
-spec get_fire_list(PN::#petri_net{}, M::list({atom(), integer()}))-> [{atom(), list({atom(), integer()})}].
get_fire_list(PN, M)->
	[get_next_transition_to_fire(PN, M)].

%% @doc calculates what the next sequence of transition activations will be, and their associated markings.
%% C is a counter of how many transitions to look forward.
%% @todo: need to halt at the next non-live generation
%% @spec get_fire_list(PN::#petri_net{}, M::list({atom(), integer()}), C::integer())-> [{atom(), list({atom(), integer()})}]
-spec get_fire_list(PN::#petri_net{}, M::list({atom(), integer()}), C::integer())-> [{atom(), list({atom(), integer()})}].
get_fire_list(_PN, _M, 0)->
	[];
get_fire_list(PN, M, C)->
	{Tl, Mn} = get_next_transition_to_fire(PN, M),
	[{Tl, Mn}] ++ get_fire_list(PN, Mn, C-1).
