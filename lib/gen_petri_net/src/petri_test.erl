-module(petri_test).
-compile(export_all).
-include("petrinet.hrl").

construction_1_test()->
	{ok, _PetriNet} = petrinet:new(
		[{s1,0, false}, {s2,0, false}], %	places {place_name, initial_tokens, is_inhibitor}
		[t1, t2], %	transitions
		[{in, [
			{s1,t1,1},
			{s2,t2,1}
			]},	% {in|out, {from, to, weight}}
		 {out, [
			{t1,s2,1},
			{t2,s1,1}
			]}], % arcs	
		[{s1, 1}] %	start states {place_name, initial tokens}
	).
