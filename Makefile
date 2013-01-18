ELIXIR_PATH := deps/elixir/bin

setup: get-deps compile test


get-couchdb-deps:
	cd deps && git clone -b 1.2.x git://github.com/apache/couchdb.git

get-deps:
	./rebar get-deps
	cd deps/elixir && make compile

compile: clean elixir erlang

clean:
	rm -rf ebin/ && PATH=$(PATH):$(ELIXIR_PATH) mix clean

erlang:
	./rebar compile

elixir:
	PATH=$(PATH):$(ELIXIR_PATH) mix compile


test: test_elixir


test_elixir:
	PATH=$(PATH):$(ELIXIR_PATH) mix test