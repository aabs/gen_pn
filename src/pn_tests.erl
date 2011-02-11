-module(pn_tests).
-include_lib("eunit/include/eunit.hrl").
-include("petrinet.hrl").

new1_test()->
	{#petri_net{name=blah, inarcs = [], outarcs = [], places = [], transitions = []}, _M} = petrinet:new(blah).

new2_test()->
	Places = [#place{name=p1},#place{name=p2},#place{name=p3}],
	Transitions = [#transition{name=t1}],
	Name = blah,
	{#petri_net{name=Name, places = Places, transitions = Transitions}, _M} = petrinet:new(Name, Places, Transitions).

new3_test()->
	Places = [],
	Transitions = [#transition{name=t1}],
	Name = blah,
	{#petri_net{name=Name, places = Places, transitions = Transitions}, _M} = petrinet:new(Name, Places, Transitions).

new4_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=2},
			  #place{name=p3, initial_marking=3}],
	Transitions = [#transition{name=t1}],
	Name = blah,
	{_PN, M} = petrinet:new(Name, Places, Transitions),
	?assert({p1, 1} =:= lists:keyfind(p1, 1, M)),
	?assert({p2, 2} =:= lists:keyfind(p2, 1, M)),
	?assert({p3, 3} =:= lists:keyfind(p3, 1, M)),
	?assert(false =:= lists:keyfind(p4, 1, M)).

new5_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=2},
			  #place{name=p3, initial_marking=3}],
	Transitions = [#transition{name=t1}],
	Name = blah,
	{PN, _M} = petrinet:new(Name, Places, Transitions),
	PN2 = petrinet:add_arc(PN, t1, #arc{place = p1}, in),
%	[#arc{place=p1, inhibitor=false,weight=1}] = petrinet:get_arcs(PN2, t1, in).
	io:format("arcs: ~p~n", [petrinet:get_arcs(PN2, t1, in)]).

addplace1_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=2},
			  #place{name=p3, initial_marking=3}],
	Transitions = [#transition{name=t1}],
	Name = blah,
	{PN, _M} = petrinet:new(Name, Places, Transitions),
	{place, p1, infinity, 1} = lists:keyfind(p1, 2, PN#petri_net.places),
	{place, p2, infinity, 2} = lists:keyfind(p2, 2, PN#petri_net.places),
	{place, p3, infinity, 3} = lists:keyfind(p3, 2, PN#petri_net.places),
	?assertNot(lists:keyfind(p4, 1, PN#petri_net.places)),
	PN2 = petrinet:add_places([#place{name=p4, initial_marking=4}], PN),
	{place, p1, infinity, 1} = lists:keyfind(p1, 2, PN2#petri_net.places),
	{place, p2, infinity, 2} = lists:keyfind(p2, 2, PN2#petri_net.places),
	{place, p3, infinity, 3} = lists:keyfind(p3, 2, PN2#petri_net.places),
	{place, p4, infinity, 4} = lists:keyfind(p4, 2, PN2#petri_net.places).


addtran1_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=2},
			  #place{name=p3, initial_marking=3}],
	Transitions = [#transition{name=t1}],
	Name = blah,
	{PN, _M} = petrinet:new(Name, Places, Transitions),
	?assertMatch(#transition{name = t1}, lists:keyfind(t1, 2, PN#petri_net.transitions)),
	?assertNot(lists:keyfind(t2, 1, PN#petri_net.transitions)),
	?assertNot(lists:keyfind(t1, 2, PN#petri_net.inarcs)),
	?assertNot(lists:keyfind(t2, 2, PN#petri_net.outarcs)),
	PN2 = petrinet:add_transitions([#transition{name=t2}], PN),
	?assert(false =/= lists:keyfind(t1, 1, PN2#petri_net.inarcs)),
	?assert(false =/= lists:keyfind(t2, 1, PN2#petri_net.outarcs)),
	
	?assertMatch(#transition{name = t1, priority=1}, lists:keyfind(t1, 2, PN2#petri_net.transitions)),
	?assertMatch(#transition{name = t2, priority=1}, lists:keyfind(t2, 2, PN2#petri_net.transitions)).

marking1_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=2},
			  #place{name=p3, initial_marking=3}],
	Transitions = [#transition{name=t1},#transition{name=t2}],
	Name = blah,
	{PN, M} = petrinet:new(Name, Places, Transitions),
	PN2 = petrinet:add_arcs(PN, t1, [#arc{place = p1},#arc{place = p2, inhibitor = true}], in),
	[{p1,1,1,false, true}, {p2, 2, 1, true, false}] = petrinet:get_markings_for_transition(t1, PN2, M).

enabled_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=2},
			  #place{name=p3, initial_marking=3}],
	Transitions = [#transition{name=t1},#transition{name=t2}],
	Name = blah,
	{PN, M} = petrinet:new(Name, Places, Transitions),
	PN2 = petrinet:add_arcs(PN, t1, [#arc{place = p1},#arc{place = p2, inhibitor = true}], in),
	?assertMatch(false, petrinet:transition_is_enabled(t1,PN2,M)).

enabled2_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=2},
			  #place{name=p3, initial_marking=3},
			  #place{name=p4, initial_marking=1},
			  #place{name=p5, initial_marking=2},
			  #place{name=p6, initial_marking=3},
			  #place{name=p7, initial_marking=1},
			  #place{name=p8, initial_marking=2},
			  #place{name=p9, initial_marking=3}],
	Transitions = [#transition{name=t1},#transition{name=t2,priority=2},#transition{name=t3,priority=3}],
	Name = blah,
	{PN, M} = petrinet:new(Name, Places, Transitions),
	PN2 = petrinet:add_arcs(PN, t1, [#arc{place = p1},#arc{place = p2}], in),
	PN3 = petrinet:add_arcs(PN2, t2, [#arc{place = p4},#arc{place = p5}], in),
	PN4 = petrinet:add_arcs(PN3, t3, [#arc{place = p7},#arc{place = p8}], in),
	?assertMatch(true, petrinet:transition_is_enabled(t1,PN4,M)),
	?assertMatch(true, petrinet:transition_is_enabled(t2,PN4,M)),
	?assertMatch(true, petrinet:transition_is_enabled(t3,PN4,M)),
	?assertMatch([#transition{name=t3,priority=3},#transition{name=t2,priority=2},#transition{name=t1,priority=1}], 
				 petrinet:enabled_transitions(PN4,M))
	.

enabled3_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=2},
			  #place{name=p3, initial_marking=3},
			  #place{name=p4, initial_marking=1},
			  #place{name=p5, initial_marking=2},
			  #place{name=p6, initial_marking=3},
			  #place{name=p7, initial_marking=1},
			  #place{name=p8, initial_marking=2},
			  #place{name=p9, initial_marking=3}],
	Transitions = [#transition{name=t1},#transition{name=t2,priority=2},#transition{name=t3,priority=3}],
	Name = blah,
	{PN, M} = petrinet:new(Name, Places, Transitions),
	PN2 = petrinet:add_arcs(PN, t1, [#arc{place = p1,weight=6},#arc{place = p2}], in),
	PN3 = petrinet:add_arcs(PN2, t2, [#arc{place = p4},#arc{place = p5}], in),
	PN4 = petrinet:add_arcs(PN3, t3, [#arc{place = p7},#arc{place = p8,inhibitor=true}], in),
	?assertNot(petrinet:transition_is_enabled(t1,PN4,M)),
	?assertMatch(true, petrinet:transition_is_enabled(t2,PN4,M)),
	?assertNot(petrinet:transition_is_enabled(t3,PN4,M)),
	?assertMatch([#transition{name=t2,priority=2}],petrinet:enabled_transitions(PN4,M)).

enabled4_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=2},
			  #place{name=p3, initial_marking=3},
			  #place{name=p4, initial_marking=1},
			  #place{name=p5, initial_marking=2},
			  #place{name=p6, initial_marking=3},
			  #place{name=p7, initial_marking=1},
			  #place{name=p8, initial_marking=0},
			  #place{name=p9, initial_marking=3}],
	Transitions = [#transition{name=t1},#transition{name=t2,priority=2},#transition{name=t3,priority=3}],
	Name = blah,
	{PN, M} = petrinet:new(Name, Places, Transitions),
	PN2 = petrinet:add_arcs(PN, t1, [#arc{place = p1,weight=6},#arc{place = p2}], in),
	PN3 = petrinet:add_arcs(PN2, t2, [#arc{place = p4},#arc{place = p5}], in),
	PN4 = petrinet:add_arcs(PN3, t3, [#arc{place = p7},#arc{place = p8,inhibitor=true}], in),
	?assertNot(petrinet:transition_is_enabled(t1,PN4,M)),
	?assertMatch(true, petrinet:transition_is_enabled(t2,PN4,M)),
	?assertMatch(true, petrinet:transition_is_enabled(t3,PN4,M)),
	X = petrinet:enabled_transitions(PN4,M),
	?assertMatch([#transition{name=t3,priority=3},#transition{name=t2,priority=2}],X).

fire1_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=1},
			  #place{name=p3, initial_marking=1},
			  #place{name=p4, initial_marking=0}],
	Transitions = [#transition{name=t1}],
	Name = blah,
	{PN, M} = petrinet:new(Name, Places, Transitions),
	PN2 = petrinet:add_arcs(PN, t1, [#arc{place = p1},#arc{place = p2},#arc{place = p3}], in),
	PN3 = petrinet:add_arcs(PN2, t1, [#arc{place = p4}], out),
	FireList = petrinet:get_fire_list(PN3, M),
	?assertMatch([{t1, [{p1, 0}, {p2, 0}, {p3, 0}, {p4, 1}]}], FireList).

fire2_test()->
	Places = [
			  #place{name=p1, initial_marking=1},
			  #place{name=p2, initial_marking=1},
			  #place{name=p3, initial_marking=1},
			  #place{name=p4, initial_marking=0}],
	Transitions = [#transition{name=t1}],
	Name = blah,
	{PN, M} = petrinet:new(Name, Places, Transitions, [{t1, in, [#arc{place = p1},#arc{place = p2},#arc{place = p3}]},
								 {t1, out, [#arc{place = p4}]}]),
	FireList = petrinet:get_fire_list(PN, M),
	?assertMatch([{t1, [{p1, 0}, {p2, 0}, {p3, 0}, {p4, 1}]}], FireList).

gen_test()->
	{ok, Pid} = sample_pn:start(mypn),
	Pid ! {signal, p1, []}.