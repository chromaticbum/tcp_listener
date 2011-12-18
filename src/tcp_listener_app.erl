-module(tcp_listener_app).
-behavior(application).

-export([
    start/2,
    stop/1
  ]).

start(_Type, _Args) ->
  tcp_server_sup:start_link(),
  tcp_listener_sup:start_link().

stop(_S) ->
  ok.

