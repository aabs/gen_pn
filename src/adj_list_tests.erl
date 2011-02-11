-module(adj_list_tests).
-include_lib("eunit/include/eunit.hrl").
-include("petrinet.hrl").

new_1_test()->
	AL = adj_list:new(blah),
	?assert(adj_list:get_adjs(AL) =:= []).

add_item_test() ->
	Expected = [hello, world, eric, ernie],
	AL = adj_list:new(blah),
	AL2 = adj_list:add_adjs(Expected, AL),
	X = adj_list:get_adjs(AL2),
	?assert( lists:all(fun(Y)-> lists:any( fun(Z) -> Y =:= Z end, X) end, Expected)).

add_item_2_test() ->
	Expected = [hello, hello, world, eric, ernie],
	AL = adj_list:new(blah),
	AL2 = adj_list:add_adjs(Expected, AL),
	X = adj_list:get_adjs(AL2),
	?assert( lists:all(fun(Y)-> lists:any( fun(Z) -> Y =:= Z end, X) end, Expected)).

add_item_3_test() ->
	Expected = [hello, world, eric, ernie],
	AL = adj_list:new(blah),
	AL2 = lists:foldl(fun(X, AccIn)-> adj_list:add_adj(X, AccIn) end, AL, Expected),
	X = adj_list:get_adjs(AL2),
	?assert( lists:all(fun(Y)-> lists:any( fun(Z) -> Y =:= Z end, X) end, Expected)).

add_item_4_test() ->
	Expected = [hello, hello, world, eric, ernie],
	AL = adj_list:new(blah),
	AL2 = lists:foldl(fun(X, AccIn)-> adj_list:add_adj(X, AccIn) end, AL, Expected),
	X = adj_list:get_adjs(AL2),
	?assert( lists:all(fun(Y)-> lists:any( fun(Z) -> Y =:= Z end, X) end, Expected)).

add_item_5_test() ->
	Expected = [{place, 1, 2, 3}, {place, 2, 3, 4}],
	AL = adj_list:new(tran1),
	AL2 = lists:foldl(fun(X, AccIn)-> adj_list:add_adj(X, AccIn) end, AL, Expected),
	X = adj_list:get_adjs(AL2),
	?assert( lists:all(fun(Y)-> lists:any( fun(Z) -> Y =:= Z end, X) end, Expected)).

new_2_test() ->
	Expected = [{place, 1, 2, 3}, {place, 2, 3, 4}],
	AL = adj_list:new(tran1, Expected),
	X = adj_list:get_adjs(AL),
	?assert( lists:all(fun(Y)-> lists:any( fun(Z) -> Y =:= Z end, X) end, Expected)).

new_3_test() ->
	Expected = [],
	AL = adj_list:new(tran1, Expected),
	X = adj_list:get_adjs(AL),
	?assert( lists:all(fun(Y)-> lists:any( fun(Z) -> Y =:= Z end, X) end, Expected)).
