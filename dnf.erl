-module (dnf,[ValParser,ValSerializer]).
%-export ([dnf/1, print/1, print_str/1, parse/1, make/1]).
-compile( export_all ).

parse(L) ->
	P = propser:new(ValParser,ValSerializer),
	P:parse(L).
serialize(F) ->
	P = propser:new(ValParser,ValSerializer),
	P:serialize(F).

%%%%% Parse, apply DNF and print a formula
make(Expr) -> 
	P = propser:new(ValParser,ValSerializer),
	P:serialize( dnf( P:parse(Expr) ) ).

%%%%% Apply de morgan, remove double negation and apply DNF to a (parsed) formula
dnf(Formula) ->
%	io:format('start: ~p~n',[Formula]),
	Formula1 = apply_de_morgan(Formula)
%	, io:format('f1: ~p~n',[Formula1])
	,Formula2 = remove_double_neg(Formula1)
%	, io:format('f2: ~p~n',[Formula2])
	,apply_dnf(Formula2).
%	, io:format('f3: ~p~n',[Formula3])
%	,Formula3.
	% ,prune(Formula3).

apply_de_morgan(Formula) ->
	apply_de_morgan(should_apply_de_morgan(Formula),Formula).
apply_de_morgan(false,Formula) -> Formula;
apply_de_morgan(true,Formula) ->
	apply_de_morgan(de_morgan(Formula)).

should_apply_de_morgan({$!,{Type,_,_}}) when Type=:=$& orelse Type=:=$| -> true;
should_apply_de_morgan({$!,Cl}) -> should_apply_de_morgan(Cl);
should_apply_de_morgan({_,Fst,Snd}) ->
	case should_apply_de_morgan(Fst) of
		true -> true;
		false -> should_apply_de_morgan(Snd)
	end;
should_apply_de_morgan(_) -> false.

de_morgan({$&,Fst,Snd}) ->
	{$&,apply_de_morgan(Fst),apply_de_morgan(Snd)};
de_morgan({$|,Fst,Snd}) ->
	{$|,apply_de_morgan(Fst),apply_de_morgan(Snd)};
de_morgan({$!,{$&,Fst,Snd}}) ->
	{$|,{$!,apply_de_morgan(Fst)},{$!,apply_de_morgan(Snd)}};
de_morgan({$!,{$|,Fst,Snd}}) ->
	{$&,{$!,apply_de_morgan(Fst)},{$!,apply_de_morgan(Snd)}};
de_morgan({$!,R}) ->
	{$!,apply_de_morgan(R)};
de_morgan(V) -> V.


remove_double_neg({$!,{$!,Cl}}) -> remove_double_neg(Cl);
remove_double_neg({Type,Fst,Snd}) when Type=:=$& orelse Type=:=$| -> {Type,remove_double_neg(Fst),remove_double_neg(Snd)};
remove_double_neg({$!,Cl}) -> {$!,remove_double_neg(Cl)};
remove_double_neg(Atom) -> Atom.


apply_dnf({$|,Fst,Snd}) ->
	{$|,apply_dnf(Fst),apply_dnf(Snd)};
apply_dnf({$&,Fst,Snd}) ->
	Fst1 = apply_dnf(Fst),
	Snd1 = apply_dnf(Snd),
	[F|R] = product(Fst1,Snd1),%[[$&,X,Y]||X<-Fst1,Y<-Snd1],
%	io:format('and dnf:~n  ~p~n  ~p~n  ~p~n',[Fst1,Snd1,P]),
	to_bin(R,F);
apply_dnf({$!,S}) -> {$!,apply_dnf(S)};
apply_dnf(V) -> V.

to_bin([],O) -> O;
to_bin([Hd|Tail],O) -> to_bin(Tail,{$|,O,Hd}).

product(L,R) -> product(L,R,[]).
product({$|,LFst,LSnd},Right,Out) ->
	ToAdd = product(LFst,Right,[]),
	product(LSnd,Right,ToAdd ++ Out);
product(Clause,{$|,RFst,RSnd},Out) ->
	ToAdd = product(Clause,RFst,[]),
	product(Clause,RSnd,ToAdd ++ Out);
product(LClause,RClause,Out) -> [{$&,LClause,RClause}|Out].
