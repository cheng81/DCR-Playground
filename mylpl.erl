-module (mylpl).
-define (KEYS, [?KEY_CTR,?KEY_BND]).
-define (KEY_BND, mylpl_bndvars).
-define (KEY_CTR, mylpl_ctr).

-include ("mylpl.hrl").
-export ([exec/2, exec_all/2]).
%-export ([wrap_exec/2, wrap_exec_all/2]).
-export ([wrap/1, wrap_fn/2]).
-export ([init_dictionary/0, reset_dictionary/0]).
%-export ([test_wrap_exec_1/0]).

-ifdef (debug).
-define (LOG (M,T), io:format("~s: ~p~n",[M,T])).
-else.
-define (LOG (M,T), true).
-endif.

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Util Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% wrap a function to a partial function - does not fail when called with wrong clause
% this is usedul to avoid writing tedious code such as:
% fun({A1,B1,Op1},{A2,B2,Op2}) -> ?And(B1==A2,?And(Op1==Op2),?And(not(A1==B2),?ADD({A1,B2,Op1}))) end
% ..just to make sure that the erlang runtime does not fail saying that something is wrong.. instead,
% one could write:
% mypl:wrap( fun( {A,B,Op}, {B,C,Op} ) when not(A==C) -> ?ADD( {A,C,Op} ) end )
% notice that we still needs to say that A!=C, but that's standard erlang, and the overall code is 
% much cleaner
%
wrap(Fn) when is_function(Fn) -> {ar(Fn),wrap_fn(Fn,?NOTAPP)}.

init_dictionary() -> lists:foreach(fun(K) -> put(K,init_dict_val(K)) end, ?KEYS).
reset_dictionary() -> lists:foreach(fun(K) -> erase(K) end, ?KEYS).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_dict_val(?KEY_CTR) -> 0;
init_dict_val(?KEY_BND) -> [].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Wrap the function and exec against the list of Facts
%
% wrap_exec(Fn,Facts) ->
% 	exec( wrap(Fn) ,Facts ).
% wrap_exec_all(Fns,Facts) ->
% 	exec_all( lists:map(fun wrap/1,Fns), Facts ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% runs the queries in Qs,
% until no one produce any update -or results in bot.
%
exec_all(Qs,Facts) when is_list(Qs) ->
	Qss = [ wrap(Q) || Q <- Qs ],
	exec_all(Qss,Qss,Facts,0).

exec_all(_,_,{bot,_}=Bot,_) -> Bot;
exec_all([],_,Facts,0) -> Facts;
exec_all([],Qs,Facts,_) -> exec_all(Qs,Qs,Facts,0);
exec_all([Q|R],Qs,Facts,N) ->
	Upd = exec(Q,Facts),
	N1 = case Upd of
		{bot,_} -> 0;
		_ -> length( subtract(Upd,Facts) ) + length( subtract(Facts,Upd) )
	end,
	exec_all(R,Qs,Upd,N+N1).

%
% runs the query Q against the list of facts Facts
% out: bot|NewFacts
%
exec({Arity,Q},Facts) when is_function(Q) ->
	exec(Arity,Q,Facts,start);
exec(Q,Facts) when is_function(Q) ->
	exec(wrap(Q),Facts).

%
% This should be the preferred way to interact
% with mylpl. Expand and Check functions are
% run first. Then if the result of check is not bot,
% reduce functions are called.
%
sysexec({Expand,Check,Reduce},Facts) ->
	%MapWrap = fun(List) -> [wrap(El) || El <- List] end,
	%Expand = MapWrap(Expand0), Check = MapWrap(Check0), Reduce = MapWrap(Reduce0),
	case exec_all( Check, exec_all(Expand,Facts) ) of
		{bot,_}=Bot -> Bot;
		Facts1 -> exec_all(Reduce,Facts1)
	end.

%
% check that Facts1 subsumes Facts2
%
subsumes(Funs,Facts1,Facts2) ->
	case sysexec(Funs,Facts1++Facts2) of
		{bot,_}=Bot -> {false,Bot};
		_	->
			FFacts1 = sysexec(Funs,Facts1),
			FFacts2 = sysexec(Funs,Facts2),
			Res = sysexec(Funs,FFacts1++FFacts2),
			{ lists:all( fun(Fact) -> has_eq(FFacts1,Fact) end, Res ),Res }
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Internals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exec(_Ar,_Fn,_Facts,{bot,_}=Bot) -> Bot;			% last update was bot! Q failed!
exec(_Ar,_Fn,Facts,{[],[]}) -> Facts;				% last update changed nothing, return Facts
exec(Ar,Fn,Facts,Upd) ->							% normal case, update facts and re-iterate
	NewFacts = unionsubtract(Facts,Upd),
	Candidates = product_n(Ar,NewFacts),
	Reactions = run(Fn,Candidates),
	exec( Ar,Fn,NewFacts, fail_or_update(Reactions,NewFacts) )
	.

unionsubtract(A,start) -> A;
unionsubtract(A,{Add,Rm}) -> subtract( union( A,Add ), Rm ).
union(A,B) -> sets:to_list( sets:union( sets:from_list(A),sets:from_list(B) ) ). %lists:append(F,Add).
subtract(A,B) -> sets:to_list( sets:subtract( sets:from_list(A),sets:from_list(B) ) ).

run(Fn,Candidates) -> lists:filter( fun(El) -> not(El==?NOTAPP) end, lists:flatten(lists:map( Fn,Candidates )) ).
% run_callme(Fn,Candidates) ->
% 	lists:filter( fun(El) -> not(El==?NOTAPP) end, lists:map(
% 	fun(El) -> callme(Fn,El) end
% 	,Candidates) ).
% 
% callme(Fn,El) when is_list(El) ->
% 	try apply(Fn,El) of Res -> Res
% 		catch error:function_clause -> ?NOTAPP
% 	end;
% callme(Fn,El) ->
% 	try apply(Fn,[El]) of Res -> Res
% 		catch error:function_clause -> ?NOTAPP
% 	end.

newvar(BndTo) ->
	Bounds = get(?KEY_BND),
	newvar(BndTo, proplists:get_value(BndTo,Bounds), Bounds ).

newvar(BndTo,undefined,Vars) ->
	Val = get(?KEY_CTR), put( ?KEY_CTR,Val+1 ),
	put(?KEY_BND, [{BndTo,{var,Val}}|Vars]),
	{var,Val};
newvar(_,Bound,_) -> Bound.

fail_or_update(Reactions,Facts) ->
	case lists:any( fun({Cmd,_}) -> Cmd==?FAIL_C end, Reactions ) of
		true -> {bot,lists:filter(fun({Cmd,_}) -> Cmd==?FAIL_C end,Reactions)};
		false ->
			{Add,Rm} = update(Reactions,[],[]),
			{subtract(Add,Facts),intersect(Rm,Facts)}
	end.

update([],Add,Rm) -> {Add,Rm};
update([{?ADD_C,El}|R],Add,Rm) -> ?LOG("ADD",El), update(R,[El|Add],Rm);
update([{?RM_C,El}|R],Add,Rm) -> ?LOG("RM",El), update(R,Add,[El|Rm]);
update([El|R],Add,Rm) -> ?LOG("What's this",El), update(R,Add,Rm).

intersect(A,B) -> sets:to_list( sets:intersection( sets:from_list(A),sets:from_list(B) ) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% wrap a fn and a def return, avoid error:function_clause
wrap_el(El) when is_list(El) -> El;
wrap_el(El) -> [El].
wrap_fn(Fn,Def) ->
	fun(El) ->
		try apply(Fn,wrap_el(El)) of Res -> Res 
		catch error:function_clause -> Def 
		end 
	end.

% n cartesian product
% product_n(4,L) -> L x L x L x L
product_n(N,L) -> product_n(N,L,[]).
product_n(0,_,Out) -> Out;
product_n(N,L,[]) -> product_n(N-1,L,L);
product_n(N,L,O) ->
	product_n(N-1,L,[ lists:flatten([X|[Y]]) || X<-L,Y<-O ]).

% get function arity
ar(F) -> proplists:get_value(arity,erlang:fun_info(F)).


% search for an equivalent fact
%% TODO: for now it just checks if there is an exaclty equal tuple..
has_eq([],_) -> false;
has_eq([A|_],A) -> true;
has_eq([_|T],A) -> has_eq(T,A).


% print message and some erlang term
%p(M,T) -> io:format("~s: ~p~n",[M,T]).





%%%%%%%%%%%%%%%%%%%
%TESTS

% transitive(eq) -> true;
% transitive(_) -> false.
% test_wrap_exec_1() ->
% 	Res0 = exec(
% 	fun({A,B,Op},{B,C,Op}) when not(A==C) -> ?If((transitive(Op)),?ADD({A,C,Op})) end
% 	,[{a,b,eq},{b,c,eq}]),
% 	p("res",Res0)
% 	.

te_trns({A,B,eq},{B,C,eq}) -> ?ADD({A,C,eq}).
te_comm({A,B,eq}) -> ?ADD({B,A,eq}).
tc_eq({A,B,eq},{A,B,neq}) -> ?FAIL({A,B,[eq,neq]}).
tc_eqv({{v,V},{v,V1},eq}) when V/=V1 andalso V/=ast andalso V1/=ast -> ?FAIL({V,V1,eq}).
tc_neqv({{v,V},{v,V},neq}) when V/=ast -> ?FAIL({V,V,neq}).
tr_eqv({{v,_},_,_}=RM) -> ?RM(RM).
tr_eq({A,A,eq}=RM) -> ?RM(RM).
tr_trns({A,B,eq},{B,A,eq}=RM) -> ?RM(RM).
tr_trnsv({A,{v,_}=V,eq},{A,V1,eq}=RM) when V/=V1 -> ?RM(RM).
test_runsub(F1,F2,Log,LogArgs) ->
	Res = subsumes( {
	[fun te_trns/2,fun te_comm/1],
	[fun tc_eq/2,fun tc_eqv/1,fun tc_neqv/1],
	[fun tr_eq/1,fun tr_eqv/1, fun tr_trnsv/2]}
	,F1,F2 ),
	?LOG(Log,LogArgs),
	?LOG("Res",Res).

test_subsume0() ->
	F1 = [{a,{v,5},eq}],
	F2 = [{b,a,eq}],
	test_runsub(F1,F2,"Expected false, subsumes",[F1,F2]).

test_subsume1() ->
	F1 = [{a,{v,5},eq},{b,{v,3},eq}],
	F2 = [{a,{v,5},eq}],
	test_runsub(F1,F2,"Expected true, subsumes",[F1,F2]).

test_subsume2() ->
	F1 = [{a,{v,5},eq},{b,{v,5},eq}],
	F2 = [{a,b,eq}],
	test_runsub(F1,F2,"Expected true, subsumes",[F1,F2]).

test_subsume3() ->
	F1 = [{a,{v,5},eq},{b,{v,3},eq}],
	F2 = [{a,b,eq}],
	test_runsub(F1,F2,"Expected false (bot), subsumes",[F1,F2]).





