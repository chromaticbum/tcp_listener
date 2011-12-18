-module(tcp_listener_sup).
-behavior(supervisor).

-export([
    start_link/0,
    start_child/2
  ]).

-export([
    init/1
  ]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Port, Module) ->
  supervisor:start_child(?SERVER, [Port, Module]).

init([]) ->
  TcpListener = {tcp_listener, {tcp_listener, start_link, []},
    temporary, 2000, worker, [tcp_listener]},
  Children = [TcpListener],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
