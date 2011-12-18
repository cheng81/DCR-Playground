% Add the fact El
-define(ADD (El), {add,El}).
% Remove the fact El
-define(RM (El), {rm,El}).
% Fail because of fact El
-define(FAIL (El), {fail_match,El}).

-define (If (Test,Cont), (case Test of false -> ?NOTAPP; true -> Cont end)).
-define (Bnd (Expr), ?ADD({new_bnd,Expr})).

-define (ADD_C, add).
-define (RM_C, rm).
-define (FAIL_C, fail_match).

-define (NOTAPP, {not_app,ko}).

