ELIXIR_PATH := deps/elixir/bin

setup: get-deps compile

get-deps:
	./rebar get-deps
	cd deps && git clone -b 1.2.x git://github.com/apache/couchdb.git
	cd deps/elixir && make compile


compile: elixir erlang

erlang:
	./rebar compile

elixir:
	PATH=$(PATH):$(ELIXIR_PATH) mix compile


test: test_elixir


test_elixir:
	PATH=$(PATH):$(ELIXIR_PATH) mix test
