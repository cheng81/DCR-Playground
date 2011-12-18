-module (dcrde).
-include ("dcrde.hrl").

-compile( export_all ).

cpred(P) ->
	D=ssat:dnf(),
	D:parse(P).

condition(#dcr{conditions=C}=DCR,From,To,Pred) ->
	DCR#dcr{conditions=[{From,To,cpred(Pred)}|C]}.
milestone(#dcr{milestones=M}=DCR,From,To,Pred) ->
	DCR#dcr{milestones=[{From,To,cpred(Pred)}|M]}.
response(#dcr{responses=R}=DCR,From,To,Pred) ->
	DCR#dcr{responses=[{From,To,cpred(Pred)}|R]}.
includes(#dcr{includes=I}=DCR,From,To,Pred) ->
	DCR#dcr{includes=[{From,To,cpred(Pred)}|I]}.
excludes(#dcr{excludes=E}=DCR,From,To,Pred) ->
	DCR#dcr{excludes=[{From,To,cpred(Pred)}|E]}.

init_included(#dcr{m=M,params=P}=DCR,Evts) ->
	Evts1 = [make_init_incl(E,P)||E<-Evts],
	DCR#dcr{m=M#state{included=Evts1}}.
init_responses(#dcr{m=M}=DCR,Evts) ->
	DCR#dcr{m=M#state{responses=Evts}}.

make_init_incl({_,_}=E,_) -> E;
make_init_incl(E,Params) ->
	[Fst|_] = search(Params,E),
	LFst = to_list(Fst),
	{E,cpred(LFst++"="++LFst)}.

to_list(L) when is_atom(L) -> atom_to_list(L);
to_list(L) when is_list(L) -> L.

enabled(#dcr{m=M,conditions=Conds,milestones=Miles},{Ev,_}=Evt) ->
	is_enabled(
		Evt,
		M,
		merge(filter(to,Conds,Ev)),
		filter(to,Miles,Ev)
	).

% bool,DCR %true is event has been executed
exec(#dcr{m=M,conditions=Conditions,milestones=Milestones}=DCR,{Ev,_}=Evt) ->
	case is_enabled(
			Evt,
			M,
			merge(filter(to,Conditions,Ev)),
			filter(to,Milestones,Ev)) of
		false -> {DCR,false};
		true -> {DCR#dcr{m = update(DCR,Evt)},true}
	end.

is_enabled(Evt,#state{executed=Exec,included=Included,responses=Rsps},Conds,Miles) ->
	is_included(Evt,Included) 
	andalso conditions_satisfied(Conds,Evt,Included,Exec) 
	andalso milestones_satisfied(Miles,Evt,Included,Rsps).

is_included({Ev,Data},Included) ->
	subst_sat(search(Included,Ev),Data) =/= false.

conditions_satisfied(Conds,{_,Data},Included,Executed) ->
%	io:format(">>>>>>conditions.1~n~p~n~n",[Conds]),
	Conds1 = [{CondEv,subst(CondPred,Data)}||{CondEv,CondPred}<-Conds],
%	io:format(">>>>>>conditions.2~n~p~n~n",[Conds1]),
	CondsIncl_ = [{CondEv,do_and_sat(search(Included,CondEv),CondPred)}||{CondEv,CondPred}<-Conds1],
%	io:format(">>>>>>conditions.3~n~p~n~n",[CondsIncl_]),
	CondsIncl = lists:filter(fun(false) -> false; (_) -> true end, CondsIncl_),
	Res = [do_and_sat(CondPred,all(Executed,CondEv,$|))||{CondEv,CondPred}<-CondsIncl],
%	io:format(">>>>>>conditions.4~n~p~n~n",[Res]),
	not(lists:any(fun(E) -> E=:=false end, Res)).

milestones_satisfied(Miles,{_,Data},Included,Responses) ->
	Miles1 = [{MileEv,subst(MilePred,Data)}||{MileEv,MilePred}<-Miles],
	MilesIncl_ = [{MileEv,do_and_sat(search(Included,MileEv),MilePred)}||{MileEv,MilePred}<-Miles1],
	MilesIncl = lists:filter(fun(false) -> false; (_) -> true end, MilesIncl_),
	Res = [do_and_sat(MilePred,search(Responses,MileEv))||{MileEv,MilePred}<-MilesIncl],
	not(lists:any(fun(E) -> E=/=false end, Res)).


update(#dcr{m=M}=DCR,{Ev,_}=Evt) ->
	Ex1 = [Evt|M#state.executed],
	In1 = update_included(DCR,M#state.included,filter(from,DCR#dcr.includes,Ev),filter(from,DCR#dcr.excludes,Ev),Evt),
	Re1 = update_responses(DCR,M#state.responses,filter(from,DCR#dcr.responses,Ev),Evt),
	Accept = is_accepting(In1,Re1),
	#state{
		executed=Ex1,
		included=In1,
		responses=Re1,
		accepting=Accept
	}.

update_included(_DCR,Included,Includes,Excludes,{_,Data}) ->
	MergedIncl_ = merge(Includes),
	MergedExcl_ = merge(Excludes),
	
	MergedIncl = [{Ev,subst(Pred,Data)}||{Ev,Pred}<-MergedIncl_],
	MergedExcl = [{Ev,subst({$!,Pred},Data)}||{Ev,Pred}<-MergedExcl_],
	
	io:format("Updated included:~n    Old: ~p~n    NewIncl: ~p~n    NewExcl: ~p~n~n",[Included,MergedIncl,MergedExcl]),
	
	EvsMergedIncl = sets:from_list([Ev||{Ev,_}<-MergedIncl]),
	EvsIncl = sets:from_list([Ev||{Ev,_}<-Included]),
	
	Sub = fun (A,B) -> sets:to_list(sets:subtract(A,B)) end,
	Intersect = fun (A,B) -> sets:to_list(sets:intersection(A,B)) end,
	
	InIncl_a = search_all(Included, Sub(EvsIncl,EvsMergedIncl)),
	InIncl_b = search_all(MergedIncl, Sub(EvsMergedIncl,EvsIncl)),
	
	SharedIncl = Intersect(EvsIncl,EvsMergedIncl),
	InIncl_c = [{Ev,{$|,search(Included,Ev),search(MergedIncl,Ev)}}||Ev<-SharedIncl],
	
	Included1_a = lists:append([InIncl_a,InIncl_b,InIncl_c]),
	io:format("~n~nOLD+NEWLY INCLUDED~n~p~n~n",[Included1_a]),
	
	EvsMergedExcl = sets:from_list([Ev||{Ev,_}<-MergedExcl]),
	EvsIncl_a = sets:from_list([Ev||{Ev,_}<-Included1_a]),
	
	InExcl_a = search_all(Included1_a, Sub(EvsIncl_a,EvsMergedExcl)),
	InExcl_b = search_all(MergedExcl, Sub(EvsMergedExcl,EvsIncl_a)),
	
	SharedExcl = Intersect(EvsIncl_a,EvsMergedExcl),
	InExcl_c = [{Ev,do_and_sat(search(Included1_a,Ev),search(MergedExcl,Ev))}||Ev<-SharedExcl],
	InExcl_c1 = lists:filter(fun({_,E}) -> E=/=false end,InExcl_c),
	
	O = lists:append([InExcl_a,InExcl_b,InExcl_c1]),
	io:format("~n~nFINAL NEW INCLUDED~n~p~n~n",[O]),
	
	O.

update_responses(_DCR,Re,Responses,{Ev,Data}) ->
	Re1_a = lists:filter(fun ({Ev1,Pred}) -> Ev1=/=Ev orelse false=:=subst_sat(Pred,Data) end,Re),
	lists:append([Re1_a,[{Ev1,subst(Pred,Data)}||{Ev1,Pred}<-Responses]]).

is_accepting(Included,Responses) ->
	InclResps = [do_and_sat(Pred,search(Included,Ev))||{Ev,Pred}<-Responses],
	not(lists:any(fun (E) -> E=/=false end,InclResps)).

search_all(List,Evs) ->
	search_all(List,Evs,[]).
search_all([],_,O) -> O;
search_all([{Ev,_}=P|R],Evs,O) ->
	case lists:any(fun(E)->E=:=Ev end,Evs) of
		true -> search_all(R,Evs,[P|O]);
		false -> search_all(R,Evs,O)
	end.

all(List,Ev,Conc) -> all(List,Ev,Conc,[]).
all([],_,Conc,ToMerge) -> merge_preds(ToMerge,Conc);
all([{Ev,P}|R],Ev,Conc,O) -> all(R,Ev,Conc,[to__pred(P)|O]);
all([_|R],Ev,Conc,O) -> all(R,Ev,Conc,O).
to__pred(E) -> to__pred(E,start).
to__pred([],O) -> O;
to__pred([{N,V}|R],start) -> to__pred(R,{eq,N,V});
to__pred([{N,V}|R],P) -> to__pred(R,{$&,P,{eq,N,V}}).
merge_preds(List,Conc) -> merge_preds(List,Conc,start).
merge_preds([],_,start) -> not_found;
merge_preds([],_,O) -> O;
merge_preds([H|T],Conc,start) -> merge_preds(T,Conc,H);
merge_preds([H|T],Conc,P) ->
	merge_preds(T,Conc,{Conc,P,H}).
%	merge_preds(T,Conc,"("++H++")"++Conc++P).

merge_pred(_,not_found) -> not_found;
merge_pred(not_found,_) -> not_found;
merge_pred(start,A) -> A;
merge_pred(A,B) -> {$&,A,B}.
	%"("++A++")&("++B++")".



%%%%%%%%%%
%% utils
%%%%%%%%%%

%%%% ssat interface
do_and_sat(_,not_found) -> false;
do_and_sat(not_found,_) -> false;
do_and_sat(A,B) ->
	io:format("        :::::::::    do and sat:~n~p~n~p~n",[A,B]),
	%ssat:make("("++Pred1++")&("++Pred2++")").
	ssat:sat({$&,A,B}).

do_or_sat(_,not_found) -> false;
do_or_sat(not_found,_) -> false;
do_or_sat(A,B) ->
	%ssat:make("("++Pred1++")|("++Pred2++")").
	ssat:sat({$|,A,B}).

subst(not_found,_) -> not_found;
subst(Pred,Data) ->
	ssat:substitute(Pred,Data).
subst_sat(not_found,_) -> false;
subst_sat(Pred,Data) ->
	ssat:substitute_sat(Pred,Data).


%%%% filters and so on
search([],_) -> not_found;
search([{Ev,false}|_],Ev) -> io:format("Check, something wrong: ~p is false.~n",[Ev]), not_found;
search([{Ev,Pred}|_],Ev) -> Pred;
search([_|Tl],Ev) -> search(Tl,Ev).

filter(Dir,List,Ev) -> filter(Dir,List,Ev,[]).
filter(_,[],_,O) -> O;
filter(to,[{Ev1,Ev,P}|R],Ev,O) -> filter(to,R,Ev,[{Ev1,P}|O]);
filter(from,[{Ev,Ev1,P}|R],Ev,O) -> filter(from,R,Ev,[{Ev1,P}|O]);
filter(Dir,[_|R],Ev,O) -> filter(Dir,R,Ev,O).

merge(List) -> merge(List,[]).
merge([],O) -> O;
merge([{Ev,Pred}|R],O) ->
	case search_remove(O,Ev) of
		not_found -> merge(R,[{Ev,Pred}|O]);
		{Ev,PrevPred,O1} -> merge(R,[{Ev,{$&,PrevPred,Pred}}|O1])
	end.
search_remove(List,Ev) -> search_remove(List,Ev,[]).
search_remove([],_,_) -> not_found;
search_remove([{Ev,Pred}|R],Ev,O) -> {Ev,Pred,lists:reverse(O)++R};
search_remove([P|R],Ev,O) -> search_remove(R,Ev,[P|O]).

%
%
%

exec_(DCR,Evt) ->
	{D,R} = exec(DCR,Evt),
	io:format("Executed event ~p: ~p~n",[Evt,R]),
	D.
		
test() ->
	DCR0 = #dcr{
		e=[a,b,c],
		params=[{a,[foo]},{b,[bar,baz]},{c,qoox}],
		m=#state{
			included=[{a,{eq,"foo","foo"}},{b,{eq,"bar","bar"}},{c,{eq,"qoox","qoox"}}],
			accepting=false
		}
	},
	DCR1 = condition(DCR0,a,b,"foo=bar"),
	DCR2 = condition(DCR1,b,c,"baz=qoox & !bar=qoox"),
	DCR3 = response(DCR2,a,c,"!qoox=foo"),
	p("DCRDE def~n",DCR0),
	
	DCR4 = exec_(DCR3,{a,[{foo,5}]}),
	p("exec {a,foo=5}~n",DCR4#dcr.m),
	DCR5 = exec_(DCR4,{b,[{bar,5},{baz,10}]}),
	p("exec {b,bar=5 baz=10}~n",DCR5#dcr.m),
	DCR6 = exec_(DCR5,{c,[{qoox,5}]}),
	p("exec {c,qoox=5} -- should not remove response, since qoox==foo~n",DCR6#dcr.m),
	DCR7 = exec_(DCR6,{c,[{qoox,10}]}),
	p("exec {c,qoox=10} -- should be accepting~n",DCR7#dcr.m),
	
	DCR5b = exec_(DCR4,{c,[{qoox,4}]}),
	p("{c,qoox=4} before any b, should not execute~n",DCR5b#dcr.m).

p(E) -> io:format("~p~n",[E]).
p(M,E) -> io:format(M++"~p~n",[E]).