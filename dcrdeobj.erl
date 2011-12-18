-module (dcrdeobj,[DCRProcPid]).

-export ([make/4, init/2, accepting/0, state/0, exec/1, exec_show/1, dcr/0, exit/0, enabled/1]).

make(Type,From,To,Pred) ->
	DCRProcPid ! {Type,From,To,Pred}, ok.
init(Type,Evts) when is_atom(Type) -> %incl/resp
	DCRProcPid ! {list_to_atom("init_"++atom_to_list(Type)),Evts}, ok;
init(Type,Evts) when is_list(Type) -> %incl/resp
	DCRProcPid ! {list_to_atom("init_"++Type),Evts}, ok.

exec(Event) ->
	DCRProcPid ! {exec,Event}, ok.
exec_show(Event) ->
	rcp({exec,Event,self()},executed).
enabled(Event) ->
	rcp({enabled,Event,self()},resp_enabled).
accepting() ->
	rcp({is_accepting,self()},resp_is_accepting).
state() ->
	rcp({state,self()},resp_state).
dcr() ->
	rcp({dcr,self()},resp_dcr).

exit() ->
	DCRProcPid ! {exit}.

rcp(Cmd,Expected) ->
	DCRProcPid ! Cmd,
	receive
		{Expected,Out} -> Out
		%A -> io:format("Do not understand ~p~n",[A])
	end.