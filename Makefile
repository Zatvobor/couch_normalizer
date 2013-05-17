ELIXIR_PATH := deps/elixir/bin

setup: get-deps compile test


get-couchdb-deps:
	@ cd deps && git clone -b 1.2.2 git://github.com/apache/couchdb.git

setup-dev-couchdb:
	@ cd deps/couchdb && ./bootstrap && ./configure && make dev

get-deps:
	@ ./rebar get-deps
	@ cd deps/elixir && make compile

compile: clean elixir erlang

clean:
	@ rm -rf ebin/ && PATH=$(PATH):$(ELIXIR_PATH) mix clean

erlang:
	@ ./rebar compile

elixir:
	@ PATH=$(ELIXIR_PATH):$(PATH) mix compile


test: test_elixir


test_elixir:
	@ PATH=$(ELIXIR_PATH):$(PATH) mix test