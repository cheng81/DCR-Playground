-module (dcrdenet).
-include ("dcrde.hrl").

%-export ([start/0, loop/1]).

-compile( export_all ).

start() ->
	socket_server:start(?MODULE, 8765, {?MODULE, loop}, dcrdes:start()).

p(M,P) -> io:format(M++"~n",P).

loop(Socket,State) ->
	p("Looping",[]),
	case gen_tcp:recv(Socket,0) of
		{ok,Data} ->
			p("got ~p",[Data]),
			%do something with data!
			{struct,Items}=Json = mochijson:decode(Data),
			p("parsed ~p",[Json]),
			Id = find(Items,id),
			p("Id ~p",[Id]),
			Method = list_to_atom(find(Items,method)),
			p("Method ~p",[Method]),
			Args = find(Items,args),
			p("Args ~p",[Args]),
			Res = do(State,Method,Id,Args),
			p("Res ~p",[Res]),
			%Res1 = tojson_struct(Res),
			%p("JsonRes ~p",[Res1]),
			gen_tcp:send(Socket,mochijson:encode(Res)),
			loop(Socket,State);
		{error,closed} ->
			p("Connection closed",[]),
			ok
		end.

find(L,Id) when is_atom(Id) -> find(L,atom_to_list(Id));
find([],_) -> not_found;
find([{Id,O}|_],Id) -> O;
find([_|R],Id) -> find(R,Id).

do(State,dcr,Id,_) ->
	State ! {do,self(),{Id,dcr,ok}},
	receive
		{resp,DCR} -> p("got dcr ~p~n",[DCR]), encode_dcr(DCR);
		NF -> NF
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
		NF -> NF
	end;
do(State,init,Id,{struct,Args}) ->
	Type = find(Args,type),
	Evts = find(Args,events),
	Evts1 = fromjson_structs(Evts,[name,fields]),
	p("Type: ~p~nEvts: ~p~nEvts1: ~p~n",[Type,Evts,Evts1]),
	State ! {do,self(),{Id,init,[Type,Evts1]}},
	receive
		{resp,R} -> R;
		NF -> NF
	end;
do(State,exec,Id,Evt) ->
	Evt1 = fromjson_structevt(Evt),
	p("Exec event ~p",[Evt1]),
	State ! {do,self(),{Id,exec,Evt1}},
	receive
		{resp,R} -> R;
		NF -> NF
	end;
do(State,state,Id,_) ->
	State ! {do,self(),{Id,state,ok}},
	receive
		{resp,#state{executed=Ex,included=In,responses=Re,accepting=Accept}} -> encode_state(Ex,In,Re,Accept);
		NF -> NF
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
	%{struct,[{Name,Vals}]};
%encode_event(E) ->
%	io:format("What is this?? ~p~n",[E]),
%	E.
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
	

% parse([$0|R]) ->
% 	State ! {exit,R};
% parse([$1|R]) ->
% 	{Id,R1} = readw(R),
% 
% readw(L) -> readw(L,[]).
% readw([$ |R],W) -> {lists:reverse(W),R};
% readw([C|R],W) -> readw(R,[C|W]).

