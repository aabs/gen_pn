-module(gen_pn).

-export([behaviour_info/1]).
-export([start/3,start/4,init_it/6]).
-import(error_logger, [format/2]).
-include("petrinet.hrl").

behaviour_info(callbacks) ->
	[{init,3}, {terminate,3}, {code_change,4}];
behaviour_info(_Other) ->
    undefined.

%% @doc Start an anonymous instance of a petri net
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
%% {signal, Place, From, Args}
%%     Synchronous signal event. The sender expects to receive the response
%% 
%% 
%% 
%% 
loop(Parent, Name, Marking, PN, StateData, Mod, hibernate) ->
    proc_lib:hibernate(?MODULE,wake_hib,
		[Parent, Name, Marking, PN, StateData, Mod, true]);
loop(Parent, Name, Marking, PN, StateData, Mod, Time) ->
	Ctx = [{name, Name},
		{parent, Parent},
		{original_marking, Marking},
		{petri_net, PN},
		{state_data, StateData},
		{module, Mod},
		{time, Time}],
    Msg = receive
		{'EXIT', Parent, Reason} ->
			ExitCtx = [{message, {'EXIT', Parent, Reason}}] ++ Ctx,
		    terminate(Reason, ExitCtx);
	    Input ->
		    Input
	  after Time ->
		  {'$gen_event', timeout}
	  end,
	Ctx2 = [{message, Msg}] ++ Ctx,
    handle_msg(Ctx2).

%% dispatches incoming events to the user Mod
handle_msg(Ctx)->
	Msg = proplists:lookup(message, Ctx),
	{Outcome, NewCtx} = case Msg of
		{signal, Place, Args} ->
			Ctx2 = [{place, Place}, {args, Args}] ++ Ctx,
			process_signal(Ctx2);
		{signal, Place, Args, From} ->
			Ctx2 = [{place, Place}, {args, Args}, {from, From}] ++ Ctx,
			process_signal(Ctx2);
		_Other ->
			terminate(unknown_input, Ctx)
		end,
	process_response(Outcome, NewCtx).		

process_signal(Ctx)->
	Place = proplists:lookup(place, Ctx),
	OriginalMarking  = proplists:lookup(original_marking, Ctx),
	SignalledMarking = petrinet:signal(Place, OriginalMarking),
	FireList = petrinet:get_fire_list(proplists:lookup(petri_net, Ctx), SignalledMarking),
	case FireList of
		[non_live] ->
			{no_op, Ctx};
		[{T, ResultMarking}]->
			Ctx2 = [{signalled_marking, SignalledMarking}, {fired_transition, T}, {result_marking, ResultMarking}] ++ Ctx,
			{result, Reply, Ctx3} = invoke_transition(Ctx2),
			case Reply of 
				{stop, Reason} ->
					terminate(Reason, Ctx3);
				{ok, Response, NewStateData} ->
					{fired, [{new_state_data, NewStateData},{response, Response}]++Ctx3}
			end
	end.

invoke_transition(Ctx)->
	Mod = proplists:lookup(module, Ctx),
	T = proplists:lookup(fired_transition, Ctx),
	StateData = proplists:lookup(state_data, Ctx),
	PetriNet = proplists:lookup(petri_net, Ctx),
	OriginalMarking = proplists:lookup(original_marking, Ctx),
	ResultMarking = proplists:lookup(result_marking, Ctx),
	Args = proplists:lookup(args, Ctx),
	Reply = Mod:T(StateData, PetriNet, OriginalMarking, ResultMarking, Args),
	{result, Reply, [{reply, Reply}]++Ctx}.

process_response(Outcome, Ctx)->
	case Outcome of
		no_op ->
			do_loop(Ctx);
		fired->
			% do the reply here...
			do_reply(Ctx),
			do_loop(Ctx)
	end.

do_loop(Ctx)->
	Parent = proplists:lookup(parent, Ctx),
	Name = proplists:lookup(name, Ctx),
	Mod = proplists:lookup(module, Ctx),
	Time = proplists:lookup(time, Ctx),
	NewStateData = proplists:lookup(new_state_data, Ctx),
	PetriNet = proplists:lookup(petri_net, Ctx),
	NewStateData = proplists:lookup(new_state_data, Ctx),
	ResultMarking = proplists:lookup(result_marking, Ctx),
	loop(Parent, Name, ResultMarking, PetriNet, NewStateData, Mod, Time).


do_reply(Ctx)->
	From = proplists:lookup(from, Ctx),
	Response = proplists:lookup(response, Ctx),
	From ! Response.

%%---------------------------------------------
%% stuff lifted straight out of gen_fsm
%%---------------------------------------------
terminate(Reason, Ctx) ->
	Msg = proplists:lookup(message, Ctx),
	Name = proplists:lookup(name, Ctx),
	Mod = proplists:lookup(module, Ctx),
	OriginalMarking = proplists:lookup(original_marking, Ctx),
	PetriNet = proplists:lookup(petri_net, Ctx),
	StateData = proplists:lookup(state_data, Ctx),
    case catch Mod:terminate(Reason, OriginalMarking, PetriNet, StateData) of
	{'EXIT', R} ->
	    error_info(R, Name, Msg, OriginalMarking, StateData),
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
		    error_info(Reason,Name,Msg,OriginalMarking, FmtStateData),
		    exit(Reason)
	    end
    end.

error_info(Reason, Name, Msg, Marking, StateData) ->
    Reason1 = case Reason of
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
    Str = "** Petri Net ~p terminating \n" ++
	get_msg_str(Msg) ++
	"** When Marking == ~p~n"
        "**      Data  == ~p~n"
        "** Reason for termination = ~n** ~p~n",
    format(Str, [Name, Marking, StateData, Reason1]),
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
