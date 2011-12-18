-module (runner).
-export ([start/0]).

c(N) -> compile:file(N).
start() ->
	c(mochijson),
	c(mylpl),
	c(socket_server),
	c(propser),
	c(dnf),
	c(ssat),
	c(dcrde),
	c(dcrdeobj),
	c(dcrdeproc),
	c(dcrdes),
	c(dcrdenet2),
	dcrdenet2:start().