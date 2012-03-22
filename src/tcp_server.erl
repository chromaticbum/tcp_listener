-module(tcp_server).
-author('chromaticbum@gmail.com').
-author('saleyn@gmail.com').

-include("tcp_listener.hrl").

-behaviour(gen_fsm).

-export([
    behaviour_info/1
  ]).

-export([
    create/1,
    start_link/1,
    set_socket/2
  ]).

%% gen_fsm callbacks
-export([
    init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
  ]).

%% FSM States
-export([
    socket_wait/2,
    hello/2
  ]).

-define(TIMEOUT, infinity).

behaviour_info(callbacks) ->
  [
    {create, 1},
    {terminate, 1},
    {handle_data, 2}
  ].

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

-spec create(Server) -> {ok, pid()} | ignore | {error, any()} when
  Server :: tcp_server().
create(Server) ->
  tcp_server_sup:start_child(Server).

-spec start_link(Server) -> {ok, pid()} | ignore | {error, any()} when
  Server :: tcp_server().
start_link(Server) ->
  gen_fsm:start_link(?MODULE, [Server], []).

-spec set_socket(Pid, Socket) -> ok | {error, any()} when
  Pid :: pid(),
  Socket :: inet:socket().
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
  gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([Server]) ->
  process_flag(trap_exit, true),
  {ok, socket_wait, Server}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
socket_wait({socket_ready, Socket}, State) when is_port(Socket) ->
  % Now we own the socket
  inet:setopts(Socket, [{active, once}, {packet, 4}, binary]),
  {next_state, hello, State, ?TIMEOUT};
socket_wait(Other, State) ->
  error_logger:error_msg("socket_wait received unknown data: ~p\n", [Other]),
  %% Allow to receive async messages
  {next_state, socket_wait, State}.


hello({data, Data}, #tcp_server{module = Module, info = Info} = State) ->
  case Module:handle_data(Info, Data) of
    {ok, Info2} -> {next_state, hello, State#tcp_server{info = Info2}, ?TIMEOUT};
    {stop, Reason} -> {stop, Reason, State}
  end;
hello(timeout, State) ->
  error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
  {stop, normal, State};
hello(Data, State) ->
  error_logger:warning_message("hello received unknown data: ~p~n", [Data]),
  {next_state, hello, State, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #tcp_server{socket=Socket} = StateData) ->
  % Flow control: enable forwarding of next TCP message
  error_logger:info_msg("Raw TCP packet: ~p~n", [Bin]),
  inet:setopts(Socket, [{active, once}]),
  ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
  #tcp_server{socket = Socket, ip = Ip} = StateData) ->
  error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Ip]),
  {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
  {noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #tcp_server{socket = Socket, module = Module, info = Info}) ->
  Module:terminate(Info),
  (catch gen_tcp:close(Socket)),
  ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

