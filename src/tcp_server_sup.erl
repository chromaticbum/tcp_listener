-module(tcp_server_sup).
-behavior(supervisor).

-export([
    start_link/0,
    start_child/1
  ]).

-export([
    init/1
  ]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Server) ->
  supervisor:start_child(?SERVER, [Server]).

init([]) ->
  Server = {tcp_server, {tcp_server, start_link, []},
    temporary, 2000, worker, [tcp_server]},
  Children = [Server],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

