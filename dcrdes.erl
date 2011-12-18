-module (dcrdes).
-include ("dcrde.hrl").
-export ([start/0, loop/1]).

start() ->
	spawn(?MODULE, loop, [{[],[]}]).

loop({DCRs,Listeners}) ->
	receive
		{new,Name} -> loop({[dcrdeproc:new(Name)|DCRs],Listeners});
		{listen,Id,Key,Listener} ->
			Listeners1 = case search(DCRs,Id) of
				{ok,_} -> add_listener(Listeners,Key,Listener,Id);
				not_found -> Listeners
			end,
			loop({DCRs,Listeners1});
		{unlisten_all,Key} ->
			loop({DCRs,remove_all_listeners_of(Listeners,Key)});
		{unlisten,Id,Key} ->
			loop({DCRs,remove_listener(Listeners,Key,Id)});
		{do,To,{Id,Cmd,Args}} -> 
			case search(DCRs,Id) of
				{ok,DCR} -> To ! {resp,callfn(DCR,Cmd,Args,Listeners,Id)};
				not_found -> To ! not_found
			end,
			loop({DCRs,Listeners});
		{exit,To,Id} ->
			NewSt = case search_remove(DCRs,Id) of
				{ok,DCR,DCRs1} -> To ! {resp,DCR:exit()}, notify(Listeners,Id,exited), {DCRs1,remove_all_listeners(Listeners,Id)};
				not_found -> To ! not_found, {DCRs,Listeners}
			end,
			loop(NewSt);
		{exit} -> true;
		{ids,To} ->
			To ! {resp,ids(DCRs)},
			loop({DCRs,Listeners})
	end.

add_listener(Listeners,Key,Listener,Id) ->
	[{Id,{Key,Listener}}|Listeners].
remove_all_listeners(Listeners,Id) ->
	proplists:delete(Id,Listeners).
remove_all_listeners_of(Listeners,Key) ->
	rm_all(Listeners,Key,[]).
rm_all([],_,O) -> O;
rm_all([{_,{Key,_}}|R],Key,O) -> rm_all(R,Key,O);
rm_all([H|R],Key,O) -> rm_all(R,Key,[H|O]).
remove_listener(Listeners,Key,Id) ->
	rm_lst(Listeners,Key,Id,[]).
rm_lst([],_,_,O) -> O;
rm_lst([{Id,{Key,_}}|R],Key,Id,O) -> R++O;
rm_lst([H|R],Key,Id,O) -> rm_lst(R,Key,Id,[H|O]).

notify(Listeners,Id,Msg) ->
	lists:foreach(
	fun({Id1,{_,Lst}}) when Id1=:=Id -> Lst(Msg) end
	,proplists:lookup_all(Id,Listeners)).

ids(DCRs) ->
	lists:map(
	fun (D) ->
		#dcr{name=Id} = D:dcr(),
		Id
	end,DCRs).
search([],_) -> not_found;
search([E|R],Id) ->
	case E:dcr() of
		#dcr{name=Id} -> io:format("found dcr ~p:~p~n",[Id,E]), {ok,E};
		_ -> search(R,Id)
	end.
search_remove(L,Id) -> search_remove(L,Id,[]).
search_remove([],_,_) -> not_found;
search_remove([E|R],Id,O) ->
	case E:dcr() of
		#dcr{name=Id} -> {ok,E,lists:reverse(O)++R};
		_ -> search_remove(R,Id,[E|O])
	end.

p(M,A) -> io:format(M++"~n",A).
	
callfn(D,make,[Type,From,To,Pred],_,_) ->
	p("make ~p: ~p,~p - ~p",[Type,From,To,Pred]),
	D:make(list_to_atom(Type),From,To,Pred),
	true;
callfn(D,init,[Type,Evts],_,_) ->
	p("init ~p: ~p",[Type,Evts]),
	D:init(Type,Evts),
	true;
callfn(D,exec,Evt,Listeners,Id) ->
	p("exec ~p",[Evt]),
	case D:exec_show(Evt) of
		true -> notify(Listeners,Id,{updated,Evt}), true;
		false -> false;
		W -> p("callfn.exec do not understand? ~p",[W]), false
	end;
callfn(D,enabled,[Evt],_,_) ->
	D:enabled(Evt);
callfn(D,accepting,_,_,_) ->
	D:accepting();
callfn(D,state,_,_,_) ->
	p("getting state",[]),
	D:state();
callfn(D,dcr,_,_,_) ->
	D:dcr().
