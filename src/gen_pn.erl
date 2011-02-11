-module(gen_pn).

-export([behaviour_info/1]).
-export([start/3,start/4,init_it/6]).
-import(error_logger, [format/2]).
-include("petrinet.hrl").

behaviour_info(callbacks) ->
    [{init,3}, {terminate,3}, {code_change,4}];
behaviour_info(_Other) ->
    undefined.

%% Start an anonymous instance of a petri net
%% Args must be of the form [Places, Transitions, Arcs]
%% where Places is [{place_name::term(), capacity::int, inhibitor::bool(), initial_marking::int()}]
%% where Transitions is [{transition_name::term()}]
%% where Arcs [{transition_label::atom(), type::in|out, arcs::[#arc{}]}]
start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).
%% start a named instance of a petri net
start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

%% performs initialisation of the gen_pn, calling out to the user Mod to get the initial state data
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, _Options) ->
	Name = proplists:get_value(name, Args),
	Places = proplists:get_value(places, Args),
	Transitions = proplists:get_value(transitions, Args),
	Arcs = proplists:get_value(arcs, Args),
	{PN, Marking} = petrinet:new(Name, Places, Transitions, Arcs),
	Name = name(Name0),
    
	case catch Mod:init(Args, Marking, PN) of
	{ok, StateData} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, Marking, PN, StateData, Mod, infinity);
	{stop, Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	{'EXIT', Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.

%% accessors for names of registered processes
name({local,Name}) -> Name;
name({global,Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.

%% the main event loop.
%% permissible messages are:
%% {'EXIT', Parent, Reason} => terminates the event loop
%% {signal, Place, Args} => adds a token the Place then proceeds to fire any enabled transitions
%%    Args can contain {limit_firing, Spec} where Spec is
%%     Lim :: int() - the maximum number of fires to perform before returning to the receive loop
%%     one - perform a single fire and then return to the receive loop
%%     infinite - keep firing until there are no more enabled transitions
loop(Parent, Name, Marking, PN, StateData, Mod, hibernate) ->
    proc_lib:hibernate(?MODULE,wake_hib,
		[Parent, Name, Marking, PN, StateData, Mod, true]);
loop(Parent, Name, Marking, PN, StateData, Mod, Time) ->
    Msg = receive
	      Input ->
		    Input
	  after Time ->
		  {'$gen_event', timeout}
	  end,
    handle_msg(Msg,Parent, Name, Marking, PN, StateData, Mod, Time).

%% dispatches incoming events to the user Mod
handle_msg(Msg, Parent, Name, Marking, PN, StateData, Mod, Time)->
	case Msg of
		{'EXIT', Parent, Reason} ->
		    terminate(Reason, Name, Msg, Mod, Marking, PN, StateData);
		{signal, Place, Args} ->
			M2 = petrinet:signal(Place, Marking),
			X = petrinet:get_fire_list(PN, M2),
			case X of
				[non_live] ->
					loop(Parent, Name, M2, PN, StateData, Mod, Time);
				[{T, Mn}]->
					case Mod:T(StateData, PN, Marking, M2, Args) of 
						{stop, Reason} ->
							terminate(Reason, Name, [], Mod, M2, PN, StateData);
						{ok, _Reply, NewStateData}->
							% not sure what to do with the reply just yet, so just recycle
							loop(Parent, Name, Mn, PN, NewStateData, Mod, Time)
					end
			end
	end.


%%---------------------------------------------
%% stuff lifted straight out of gen_fsm
%%---------------------------------------------
terminate(Reason, Name, Msg, Mod, Marking, PetriNet, StateData) ->
    case catch Mod:terminate(Reason, Marking, PetriNet, StateData) of
	{'EXIT', R} ->
	    error_info(R, Name, Msg, Marking, PetriNet, StateData),
	    exit(R);
	_ ->
	    case Reason of
		normal ->
		    exit(normal);
		shutdown ->
		    exit(shutdown);
 		{shutdown,_}=Shutdown ->
 		    exit(Shutdown);
		_ ->
                    FmtStateData =
                        case erlang:function_exported(Mod, format_status, 2) of
                            true ->
                                Args = [get(), StateData],
                                case catch Mod:format_status(terminate, Args) of
                                    {'EXIT', _} -> StateData;
                                    Else -> Else
                                end;
                            _ ->
                                StateData
                        end,
		    error_info(Reason,Name,Msg,Marking, PetriNet,FmtStateData),
		    exit(Reason)
	    end
    end.
error_info(Reason, Name, Msg, Marking, PetriNet, StateData) ->
    Reason1 = 
	case Reason of
	    {undef,[{M,F,A}|MFAs]} ->
		case code:is_loaded(M) of
		    false ->
			{'module could not be loaded',[{M,F,A}|MFAs]};
		    _ ->
			case erlang:function_exported(M, F, length(A)) of
			    true ->
				Reason;
			    false ->
				{'function not exported',[{M,F,A}|MFAs]}
			end
		end;
	    _ ->
		Reason
	end,
    Str = "** State machine ~p terminating \n" ++
	get_msg_str(Msg) ++
	"** When State == ~p~n"
        "**      Data  == ~p~n"
        "** Reason for termination = ~n** ~p~n",
    format(Str, [Name, get_msg(Msg), Marking, PetriNet, StateData, Reason1]),
    ok.
get_msg_str({'$gen_event', _Event}) ->
    "** Last event in was ~p~n";
get_msg_str({'$gen_sync_event', _Event}) ->
    "** Last sync event in was ~p~n";
get_msg_str({'$gen_all_state_event', _Event}) ->
    "** Last event in was ~p (for all states)~n";
get_msg_str({'$gen_sync_all_state_event', _Event}) ->
    "** Last sync event in was ~p (for all states)~n";
get_msg_str({timeout, _Ref, {'$gen_timer', _Msg}}) ->
    "** Last timer event in was ~p~n";
get_msg_str({timeout, _Ref, {'$gen_event', _Msg}}) ->
    "** Last timer event in was ~p~n";
get_msg_str(_Msg) ->
    "** Last message in was ~p~n".

get_msg({'$gen_event', Event}) -> Event;
get_msg({'$gen_sync_event', Event}) -> Event;
get_msg({'$gen_all_state_event', Event}) -> Event;
get_msg({'$gen_sync_all_state_event', Event}) -> Event;
get_msg({timeout, Ref, {'$gen_timer', Msg}}) -> {timeout, Ref, Msg};
get_msg({timeout, _Ref, {'$gen_event', Event}}) -> Event;
get_msg(Msg) -> Msg.