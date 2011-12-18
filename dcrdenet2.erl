-module (dcrdenet2).
-include ("dcrde.hrl").

-compile( export_all ).

start() ->
	socket_server:start(?MODULE, 8765, {?MODULE, connected}, dcrdes:start()).

p(M,P) -> io:format(M++"~n",P).

get_message(Socket,Buf) ->
	case gen_tcp:recv(Socket,0) of
		{ok,Data} when binary_part( Data,byte_size(Data)-2,2 )=:=<<"\r\n">> -> list_to_binary([Buf,Data]);
		{ok,Data} -> get_message(Socket,list_to_binary([Buf,Data]));
		{error,closed} -> io:format("connection closed\r\n"), false
	end.

connected(Socket,State) ->
	io:format("A Socket connected~n",[]),
	loop(Socket,State,spawn(?MODULE, writer, [Socket])).

writer(Socket) ->
	receive
		{send,Data} -> p("Writing:~n'~p'",[Data]), gen_tcp:send(Socket,Data++"\r\n"), writer(Socket);
		W -> io:format("[writer]What? ~p~n",[W]), writer(Socket)
	end.

create_listener(WriterPid,Id) ->
	fun (exited) ->
			WriterPid ! {send, mochijson:encode({struct,[{type,"listen"},{dcr,Id},{exited,true}]})};
		({updated,Ev}) ->
			WriterPid ! {send, mochijson:encode({struct,[{type,"listen"},{dcr,Id},{executed,encode_event(Ev)}]})}
	end.

loop(Socket,State,WriterPid) ->
	p("Looping",[]),
	case get_message(Socket,<<"">>) of
		false -> 
			remove_all_listeners(State),
			ok;
		Msg ->
			{struct,Items}=Json = mochijson:decode(Msg),
			p("parsed ~p",[Json]),
			Id = find(Items,id),
			p("Id ~p",[Id]),
			Args = find(Items,args),
			case method_atom(find(Items,method)) of
				not_found -> p("method was not found!",Items);
				listen -> do(State,listen,Id,create_listener(WriterPid,Id));
				Method -> Res = do(State,Method,Id,Args), WriterPid ! {send,mochijson:encode(Res)}
			end,
			loop(Socket,State,WriterPid)
	end.


method_atom(not_found) -> not_found;
method_atom(L) when is_list(L) -> list_to_atom(L).

find(L,Id) when is_atom(Id) -> find(L,atom_to_list(Id));
find([],_) -> not_found;
find([{Id,O}|_],Id) -> O;
find([_|R],Id) -> find(R,Id).

remove_all_listeners(State) ->
	State ! {unlisten_all,self()}.

do(State,listen,Id,Listener) ->
	State ! {listen,Id,self(),Listener};
do(State,unlisten,Id,_) ->
	State ! {unlisten,self(),Id},
	receive
		{resp,_} -> true;
		_ -> "not_found"
	end;

do(State,dcr,Id,_) ->
	State ! {do,self(),{Id,dcr,ok}},
	receive
		{resp,DCR} -> p("got dcr ~p~n",[DCR]), encode_dcr(DCR);
		_ -> "not_found"
	end;
do(State,ids,_,_) ->
	State ! {ids,self()},
	receive
		{resp,Ids} -> {array,Ids}
	end;
do(State,new,Id,_) ->
	State ! {new,Id},
	p("created new proc ~p",[Id]),
	true;
do(State,exit,Id,_) -> 
	State ! {exit,self(),Id}, 
	receive 
		_ -> true
	end;
do(State,make,Id,{struct,Args}) ->
	Type = find(Args,type),
	From = find(Args,from),
	To = find(Args,to),
	Pred = find(Args,pred),
	case Pred of
		not_found -> State ! {do,self(),{Id,make,[Type,From,To]}};
		_ -> State ! {do,self(),{Id,make,[Type,From,To,Pred]}}
	end,
	receive
		{resp,R} -> R;
		_ -> "not_found"
	end;
do(State,init,Id,{struct,Args}) ->
	Type = find(Args,type),
	Evts = find(Args,events),
	Evts1 = fromjson_structs(Evts,[name,fields]),
	p("Type: ~p~nEvts: ~p~nEvts1: ~p~n",[Type,Evts,Evts1]),
	State ! {do,self(),{Id,init,[Type,Evts1]}},
	receive
		{resp,R} -> R;
		_ -> "not_found"
	end;
do(State,exec,Id,Evt) ->
	Evt1 = fromjson_structevt(Evt),
	p("Exec event ~p",[Evt1]),
	State ! {do,self(),{Id,exec,Evt1}},
	receive
		{resp,R} -> R;
		_ -> "not_found"
	end;
do(State,state,Id,_) ->
	State ! {do,self(),{Id,state,ok}},
	receive
		{resp,#state{executed=Ex,included=In,responses=Re,accepting=Accept}} -> encode_state(Ex,In,Re,Accept);
		_ -> "not_found"
	end.

encode_dcr(#dcr{
name=Name,
e=Events,
params=Params,
m=#state{executed=Ex,included=In,responses=Re,accepting=Accept},
conditions=Conds,
milestones=Miles,
responses=Resps,
includes=Incls,
excludes=Excls
}) ->
	{struct,[
		{"name",Name},
		{"events",{array,Events}},
		{"params",encode_params(Params)},
		{"state",encode_state(Ex,In,Re,Accept)},
		{"conditions",encode_rels(Conds)},
		{"milestones",encode_rels(Miles)},
		{"responses",encode_rels(Resps)},
		{"includes",encode_rels(Incls)},
		{"excludes",encode_rels(Excls)}
	]}.
encode_params(Params) ->
	{array,[encode_param(P)||P<-Params]}.
encode_param({Evt,Params}) ->
	{array,[Evt,{array,Params}]}.
encode_rels(Rels) ->
	{array,[encode_rel(R)||R<-Rels]}.
encode_rel({From,To,CPred}) ->
	D = ssat:dnf(),
	{struct,[
		{"from",From},
		{"to",To},
		{"pred",D:serialize(CPred)}
	]}.
encode_state(Ex,In,Re,Accept) ->
	io:format("Encoding state: ~n~p~n~p~n~p~n",[Ex,In,Re]),
	{struct,[
		{"accepting",Accept},
		{"executed",encode_events(Ex)},
		{"included",encode_predicates(In)},
		{"responses",encode_predicates(Re)}
	]}.
encode_events(Ex) ->
	{array,[encode_event(E)||E<-Ex]}.
encode_event({Name,Vals}) ->
	{array,[Name,{struct,Vals}]}.
encode_predicates(Pr) ->
	{array,[encode_predicate(P)||P<-Pr]}.
encode_predicate({Name,P}) ->
	D=ssat:dnf(),
	{struct,[{"name",Name},{"predicate",D:serialize(P)}]}.

fromjson_structevt({array,[Name,{struct,Vals}]}) ->
	{Name,Vals}.


%tojson_struct(V) when V=:=true orelse V=:=false ->
%	{struct,[{resp,V}]};
tojson_struct(V) when is_tuple(V) ->
	tojson_struct(tuple_to_list(V));
	%{struct,[tojson_struct(X)||X<-tuple_to_list(V)]};
tojson_struct(V) when is_list(V) ->
	%{array,[tojson_struct(X)||X<-V]};
	[tojson_struct(X)||X<-V];
tojson_struct(V) when is_atom(V) -> atom_to_list(V);
tojson_struct(V) -> V.

fromjson_structs({array,Jsons},Names) -> 
	[fromjson_struct(X,Names)||X<-Jsons].

fromjson_struct({struct,Fields},Names) -> fromjson_struct(Fields,Names,{});
fromjson_struct(V,_) -> V.
fromjson_struct(_,[],O) -> O;
fromjson_struct(F,[N|R],O) ->
	V = find(F,N),
	O1 = case V of
		not_found -> O;
		{array,Elems} -> erlang:append_element(O,Elems);
		_ -> erlang:append_element(O,V)
	end,
	fromjson_struct(F,R,O1).
