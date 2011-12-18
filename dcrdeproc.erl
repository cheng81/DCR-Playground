-module (dcrdeproc).
-include ("dcrde.hrl").

-export ([start/1, loop/1, new/1]).

new(Name) ->
	Pid = start(Name),
	dcrdeobj:new(Pid).

start(Name) ->
	spawn(fun() -> loop(#dcr{name=Name}) end).

loop(DCR) ->
	receive
		{condition,From,To,Pred} -> loop(dcrde:condition(DCR,From,To,Pred));
		{response,From,To,Pred} -> loop(dcrde:response(DCR,From,To,Pred));
		{milestone,From,To,Pred} -> loop(dcrde:milestone(DCR,From,To,Pred));
		{includes,From,To,Pred} -> loop(dcrde:includes(DCR,From,To,Pred));
		{excludes,From,To,Pred} -> loop(dcrde:excludes(DCR,From,To,Pred));
		{init_events,Events} -> 
			loop(DCR#dcr{e=lists:map(fun ({E,_}) -> E end,Events),params=Events});
		{init_included,Evts} -> loop(dcrde:init_included(DCR,Evts));
		{init_responses,Evts} -> loop(dcrde:init_responses(DCR,Evts));
		
		{enabled,Evt,To} -> Rsp = dcrde:enabled(DCR,Evt), To ! {resp_enabled,Rsp}, loop(DCR);
		{exec,Evt} -> {DCR1,_} = dcrde:exec(DCR,Evt), io:format("Done executing:~p~n",[Evt]), loop(DCR1);
		{exec,Evt,To} -> {DCR1,Out} = dcrde:exec(DCR,Evt), To ! {executed,Out}, loop(DCR1);
		{is_accepting,To} -> 
			State = DCR#dcr.m,
			To ! {resp_is_accepting,State#state.accepting},
			loop(DCR);
		{dcr,To} ->
			To ! {resp_dcr,DCR},
			loop(DCR);
		{state,To} ->
			To ! {resp_state,DCR#dcr.m},
			loop(DCR);
		{exit} -> io:format("exit dcr ~p~n",[self()]), ok
		%What -> io:format("do not understand? ~p~n",[What]), loop(DCR)
	end.