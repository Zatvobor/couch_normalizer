setup: get-deps compile

get-deps:
	./rebar get-deps
	cd deps && git clone -b 1.2.x git://github.com/apache/couchdb.git


compile: elixir erlang

erlang:
	./rebar compile

elixir:
	deps/elixir/bin/mix compile


test: test_elixir


test_elixir:
	deps/elixir/bin/mix test
