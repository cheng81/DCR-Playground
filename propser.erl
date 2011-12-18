-module (propser,[ValParser,ValSerializer]).

-export ([parse/1, serialize/1, print_out/1]).

%%%%% Parse a formula
parse(Str) when is_atom(Str) -> parse(atom_to_list(Str));
parse(Str) when is_list(Str) ->
	parse(primitive,Str,[]).

parse(_,[],[Predicate]) -> Predicate;
parse(T,[$ |Str],St) -> parse(T,Str,St);
parse(primitive,[$!|Str],St) ->
	parse(primitive,Str,[$!|St]);
parse(primitive,[$(|Str],St) ->
	parse(primitive,Str,[$(|St]);
parse(primitive,Str,St) ->
	[Atom,Str1] = r(Str),
	Atom1 = ValParser(Atom),
	parse(comp,Str1,merge(Atom1,St));
parse(comp,[$)|Str],St) ->
	[[Expr],St1] = find(St,$(),
	parse(comp,Str,merge(Expr,St1));
parse(comp,[$&|Str],[Expr|St]) ->
	parse(primitive,Str,[[$&,Expr]|St]);
parse(comp,[$||Str],[Expr|St]) ->
	parse(primitive,Str,[[$|,Expr]|St]).

% merge(Expr,[]) -> [Expr];
% merge(Expr,[$!|St]) -> merge([$!,Expr],St);
% merge(Expr,[$(|_]=St) -> [Expr|St];
% merge(Expr,[[$&,E1]|St]) -> [[$&,E1,Expr]|St];
% merge(Expr,[[$|,E1]|St]) -> [[$|,E1,Expr]|St].
merge(Expr,[]) -> [Expr];
merge(Expr,[$!|St]) -> merge({$!,Expr},St);
merge(Expr,[$(|_]=St) -> [Expr|St];
merge(Expr,[[$&,E1]|St]) -> [{$&,E1,Expr}|St];
merge(Expr,[[$|,E1]|St]) -> [{$|,E1,Expr}|St].

r(S) -> r(S,[]).
r([E|_]=S,Acc) when
	E=:=$! orelse
	E=:=$& orelse
	E=:=$| orelse
	E=:=$( orelse
	E=:=$) orelse
	E=:=$ 
 -> [lists:reverse(Acc),S];
r([E],Acc) -> [lists:reverse([E|Acc]),[]];
r([E|R],Acc) -> r(R,[E|Acc]).


find(St,El) -> find(St,El,[]).
find([El|Rest],El,SoFar) -> [lists:reverse(SoFar),Rest];
find([X|Rest],El,SoFar) -> find(Rest,El,[X|SoFar]).

print_out(F) -> io:format("~s~n",[serialize(F)]).

%print_to_str(Expr) -> print_str(Expr).
%serialize([]) -> "";
serialize({$&,Fst,Snd}) -> serialize(Fst) ++ " & " ++ serialize(Snd);
serialize({$|,{$|,_,_}=Fst,Snd}) -> serialize(Fst) ++ " | (" ++ serialize(Snd) ++ ")";
%serialize({$|,Fst,{$|,_,_}=Snd}) -> "(" ++ serialize(Fst) ++ ") | " ++ serialize(Snd);
%serialize({$|,{$&,_,_}=Fst,{$|,_,_}=Snd}) -> "(" ++ serialize(Fst) ++ ") | " ++ serialize(Snd);
serialize({$|,Fst,Snd}) -> "(" ++ serialize(Fst) ++ ") | (" ++ serialize(Snd) ++ ")";
serialize({$!,{T,_,_}=Cl}) when T=:=$& orelse T=:=$| -> "!(" ++ serialize(Cl) ++ ")";
serialize({$!,Cl}) -> "!" ++ serialize(Cl);
serialize(E) -> ValSerializer(E).
