%% Author: aabs
%% Created: 31/01/2011
%% Description: TODO: Add description to adj_list
-module(adj_list).

%%
%% Include files
%%
-include("petrinet.hrl").
%%
%% Exported Functions
%%
-export([new/1,new/2,add_adjs/2,add_adj/2,get_adjs/1]).

%% new(Label)
%% Constructs a new empty adjacency list for Label
new(Label) ->
	#adj_list{label = Label, adjacents = sets:new()}.
%% new(Label, Adjs)
%% Constructs a new adjacency list for Label adding each element in Adjs as an adjacent edge
new(Label, Adjs)->
	#adj_list{label = Label, adjacents = sets:from_list(Adjs)}.

add_adjs([], AL)->
	AL;
add_adjs(Adjs, AL) when is_list(Adjs)->
	NAL = sets:union(AL#adj_list.adjacents, sets:from_list(Adjs)),
	AL#adj_list{adjacents = NAL}.

add_adj(Adj, AL)->
	AL#adj_list{adjacents = sets:add_element(Adj, AL#adj_list.adjacents)}.

get_adjs(AL)-> 
	sets:to_list(AL#adj_list.adjacents).