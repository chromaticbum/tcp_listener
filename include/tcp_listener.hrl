-record(tcp_server, {
    module :: module(),
    info :: any(),
    socket :: inet:socket(),
    ip :: inet:address()
  }).
-type tcp_server() :: #tcp_server{}.
