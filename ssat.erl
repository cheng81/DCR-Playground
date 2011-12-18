-module (ssat).
-include ("mylpl.hrl").
-define (IfM (Test,Msg,Args,Cont), (case Test of true -> p(Msg,Args), Cont; false -> ?NOTAPP end) ).

-export ([sat/1, substitute/2, substitute_sat/2]).

-export ([read/1, serialize/1]).

-export([dnf/0]).
%-export ([sat/1]).
-compile( export_all ).

%p(_,_) -> ok. 
p(M,T) -> io:format("~s: ~p~n",[M,T]).

read(Str) -> r(Str,[]).
r([],Acc) -> io:format("WTF? ~p~n",[Acc]), throw("oh shit!");
r([$=|Str],Acc) -> {eq,lists:reverse(Acc),interpret(Str)};
r([$!,$=|Str],Acc) -> {neq,lists:reverse(Acc),interpret(Str)};
r([$ |Str],Acc) -> r(Str,Acc);
r([C|Str],Acc) -> r(Str,[C|Acc]).

serialize({eq,Var,Val}) -> Var ++ "=" ++ ser_value(Val);
serialize({neq,Var,Val}) -> "!" ++ Var ++ "=" ++ ser_value(Val).
ser_value({value,L}) when is_list(L) -> "\"" ++ L ++ "\"";
ser_value({value,I}) when is_integer(I) -> integer_to_list(I);
ser_value({value,B}) when B=:=true orelse B=:=false -> atom_to_list(B);
ser_value(Ref) -> Ref.

interpret([$"|_]=S) -> {value,lists:sublist(S,2,length(S)-2)};
interpret("true") -> {value,true};
interpret("false") -> {value,false};
interpret(Str) ->
	p("     INTERPRET VALUE::",Str),
	case numeric_string(Str) of
		false -> Str;
		N -> {value,N}
	end.
numeric_string(S) ->
	try 
		list_to_integer(S)
	catch
		_:_ -> false %io:format("not an int ~s~n~p~n~p~n",[S,C,E]), false
	end.


dnf() -> dnf:new(fun read/1,fun serialize/1).

make(F) ->
	case sat(F) of
		false -> false;
		Res -> Res
	end.

substitute_sat(Formula,Values) ->
	Expr = {$&,Formula,values_to_expr(Values)},
	p("subst_sat expr",Expr),
	sat( Expr ).

values_to_expr([{Var,Val}|R]) -> values_to_expr(R,{eq,Var,{value,Val}}).
values_to_expr([],O) -> O;
values_to_expr([{Var,Val}|R],O) -> values_to_expr(R,{$&,O,{eq,Var,{value,Val}}}).
	
substitute(Formula,Values) ->
	Dnf = dnf(),
	F1 = Dnf:dnf(Formula),
	C = subst(F1,Values),
	p("dnf.out",[F1]),
	p("subst.out",[C]),
	C.

subst(C,[]) -> C;
subst(C,[{Name,Val}|R]) ->
	subst(s(Name,Val,C),R).
s(N,V,{$|,L,R}) ->
	{$|,s(N,V,L),s(N,V,R)};
s(N,V,{$&,L,R}) ->
	{$&,s(N,V,L),s(N,V,R)};
s(N,V,{$!,C}) ->
	{$!,s(N,V,C)};
% s(N,V,A) when is_atom(A) ->
% 	{Op,Var,Val} = read(atom_to_list(A)),
% 	s(N,V,{Op,Var,Val},A).
% s(N,V,{Op,Var,N},_) -> list_to_atom(atom_to_ast({Op,Var,V}));
s(N,V,{Op,Var,N}) -> {Op,Var,{value,V}};
s(_N,_V,A) -> A.

sat(Formula) ->
	Dnf = dnf(),
	%map {$!,{eq,Var,Var}} to {neq,Var,Val} and !,{neq to {eq..
	F1 = Dnf:dnf(Formula),
	p("dnf",F1),
	CLst = to_list(F1),
	CLst1 = map_neq(CLst),
	p("to_list",CLst),
	p("map_neq",CLst1),
	T = [compute(Disj)||Disj<-CLst1],
	T1 = lists:filter(fun ({bot,_}) -> false; (_) -> true end,T),
	p("ssat.computed",T1),
	T2 = reduce(T1),
	p("ssat.reduced",T2),
	O = case length(T2) of
		0 -> false;
		_ -> to_disj_ast(T2)
	end,
	p("sat.out",O),
	O.

%TODO: check that this preserves the sub-thingy
to_disj_ast(L) -> to_disj_ast(L,bot).
to_disj_ast([],O) -> O;
to_disj_ast([Hd|Tl],bot) -> to_disj_ast(Tl,to_ast(Hd));
to_disj_ast([Hd|Tl],Ast) -> to_disj_ast(Tl,{$|,Ast,to_ast(Hd)}).

to_ast(L) -> to_ast(L,bot).
to_ast([],O) -> O;
to_ast([Hd|Tl],bot) -> to_ast(Tl,atom_to_ast(Hd));
to_ast([Hd|Tl],Ast) -> to_ast(Tl,{$&,atom_to_ast(Hd),Ast}).
atom_to_ast({$!,{neq,Var,Val}}) ->
	{eq,Var,Val};
atom_to_ast({$!,{eq,_,_}}=E) -> E;
atom_to_ast({eq,_,_}=E) -> E;
atom_to_ast({neq,Var,Val}) ->
	{$!,{eq,Var,Val}}.

map_neq(L) -> map_neq(L,[]).
map_neq([],O) -> O;
map_neq([Row|R],O) ->
	map_neq(R,[lists:map(
	fun ({$!,{eq,Var,Val}}) -> {neq,Var,Val};
		({$!,{neq,Var,Val}}) -> {eq,Var,Val};
		(E) -> E
	end,Row)|O]).

to_list({T,_,_}=C) when T=:=$| orelse T=:=$& ->
	%io:format("to_list: ~p~n",[C]),
	C1 = to_list(C,top),
	%io:format("to_list (prelim): ~p~n",[C1]),
	C1;
to_list(C) -> [[C]].

to_list({$|,Left,Right},top) ->
	L = to_list(Left,$|),
	R = to_list(Right,$|),
	%io:format("top-level or: ~p~n~p~n~n",[L,R]),
	L++R;
to_list({$|,Left,Right},$|) ->
	L = to_list(Left,$|),
	R = to_list(Right,$|),
	%io:format("sub-level or: ~p~n~p~n~n",[L,R]),
	L++R;
to_list({$&,Left,Right},V) ->
	[lists:flatten(to_list(Left,V)++to_list(Right,V))];
to_list({$!,S},_) ->
	[[{$!,S}]];
to_list(A,_) ->
	[[A]].

reduce([]) -> [];
reduce([Only]) -> [Only];
reduce([Fst|R]) -> reduce(R,[Fst]).

reduce([],O) -> lists:reverse(O);
reduce([H1|R],O) ->
	O1 = lists:filter(
	fun (El) -> not(cmp(El,H1)) end
	,O),
	reduce(R,[H1|O1]).
	% case cmp(H1,H2) of
	% 	eq -> reduce(R,O);
	% 	_ -> reduce(R,[H1|O])
	% end.

cmp(A,B) when length(A)=/=length(B) -> false;
cmp(A,B) -> pwcmp(A,B).
pwcmp([],_) -> true;
pwcmp([H|T],B) ->
	case has(H,B) of
		true -> pwcmp(T,B);
		false -> false
	end.
has(_,[]) -> false;
has(E,[E|_]) -> true;
has(E,[_|R]) -> has(E,R).
	

compute(D) ->
	mylpl:init_dictionary(),
	O=case check(expand(D)) of
		{bot,Reason}=Bot -> p("bot!",Reason), Bot;
		F -> p("Simplify",F), sets:to_list(sets:from_list(simplify(F)))
	end,
	mylpl:reset_dictionary(),
	O.


neg(eq) -> neq;
neg(neq) -> eq.

check(F) ->
	mylpl:exec_all([
	fun chk_eq/2
	],F).
expand(F) ->
	mylpl:exec_all([
	fun exp_transitive/2,
	fun exp_transitive_neg/2
	],F).
simplify(F) -> 
	mylpl:exec_all([
	fun simp_rmlhsint/1,
	fun simp_rmeqvar/2,
	fun simp_rmneqeq/2
	],F).

exp_transitive( {eq,A,B},{eq,B,C} ) when not(A==C) -> ?ADD({eq,A,C}).
exp_transitive_neg( {Op1,A,B},{Op2,B,C} )
	when not(Op1==Op2) andalso not(A==C) -> ?ADD({neq,A,C}).

chk_eq( {eq,A,{value,V1}},{eq,A,{value,V2}} ) 
	when not(V1==V2) ->
		?FAIL({A,eq,V1,V2});
	%?If(is_value(V1) andalso is_value(V2),?FAIL({A,eq,V1,V2}));
chk_eq( {eq,A,V},{neq,A,V} ) -> ?FAIL({A,eq,neq,V}).

simp_rmlhsint( {_,A,_}=Rm ) -> ?If(is_value(A),?RM(Rm)).
simp_rmeqvar( {eq,A,B}=Rm,{eq,A,C} ) -> ?If(not(is_value(B)) andalso is_value(C),?RM(Rm)).
simp_rmneqeq( {eq,A,{value,_}},{neq,A,_}=Rm ) -> ?RM(Rm).

% is_value(A) when is_integer(A) -> true; %just this is supported for now
% is_value([$"|_]) -> true;
% is_value("true") -> true;
% is_value("false") -> true;
is_value({val,_}) -> true;
is_value(_) -> false.


% simp_rmdup( {Op,A,B},{Op,A,B}=R ) -> ?RM(R).