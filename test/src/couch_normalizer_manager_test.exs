Code.require_file "../../test_helper.exs", __FILE__

defmodule CouchNormalizerManagerTest do
  use ExUnit.Case


  test :lookup_next do
    Erlang.application.set_env(:couch_normalizer_manager, :registry, Erlang.ets.new(:s2, [:ordered_set, {:keypos, 1}]))

    assert CouchNormalizer.Registry.acquire("1-scenario", fn(x) -> x end) == true
    assert CouchNormalizer.Registry.acquire("2-scenario", fn(x) -> x end) == true

    {"2", "2-scenario", f}    = Erlang.couch_normalizer_manager.lookup_next("1")

    assert f.("hi there!")    == "hi there!"
    assert f.("how are you?") == "how are you?"
  end

end