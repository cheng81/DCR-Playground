-module (dnftest).
-compile( export_all ).

new() -> dnf:new(fun (A) -> A end).