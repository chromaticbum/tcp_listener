compile:
	rebar compile

test: compile
	erl -noshell -pa ebin -eval "eunit:test([], [verbose])" -s init stop

console: compile
	erl -pa ../tcp_listener/ebin -eval "application:start(tcp_listener)."

clean:
	rebar clean
