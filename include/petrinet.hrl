-record(place, {
	name::term(), 
	capacity = infinity :: integer()|infinity, 
	initial_marking=0::integer()
	}).
-record(transition, {
	name::term(),
	priority = 1 :: integer()
	}).

-record(adj_list,{
		label::term(),
		adjacents
	}).
-record(petri_net,{
	name,
	places = [] :: [#place{}],
	transitions = [] :: [#transition{}],
	inarcs = [] :: [#adj_list{}],
	outarcs = [] :: [#adj_list{}]
}).

-record(arc,{
			 place :: term(), % the remote place in each case (input or output depending on which adj_list it's in)
			 weight = 1 :: integer(),
			 inhibitor=false::boolean() % only relevant in the case of inarcs
	}).