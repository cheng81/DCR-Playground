-module(socket_server).
-author('Jesse E.I. Farmer <jesse@20bits.com>').
-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/4]).

-define(TCP_OPTIONS, [binary, {packet, line}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
        port,
        loop,
        ip=any,
        lsocket=null,
		app=null}).

start(Name, Port, Loop, AppState) ->
    State = #server_state{port = Port, loop = Loop, app = AppState},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
			io:format("listening on port ~p~n",[Port]),
            NewState = State#server_state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
			io:format("cannot listen ~p~n",[Reason]),
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    {noreply, accept(State)};
handle_cast(E,State) -> io:format("Recvd. cast ~p~n",[E]),
	{noreply, State}.

accept_loop({Server, LSocket, {M, F}, AppState}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    % Let the server spawn a new process and replace this loop
    % with the echo loop, to avoid blocking 
    gen_server:cast(Server, {accepted, self()}),
	io:format("Socket connected, call app module"),
    M:F(Socket,AppState).
    
% To be more robust we should be using spawn_link and trapping exits
accept(State = #server_state{lsocket=LSocket, loop = Loop, app=AppState}) ->
    proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket, Loop, AppState}]),
    State.

% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.