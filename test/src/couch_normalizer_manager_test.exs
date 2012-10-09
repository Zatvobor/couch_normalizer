Code.require_file "../../test_helper.exs", __FILE__

defmodule CouchNormalizerManagerTest do
  use ExUnit.Case, async: false


  def setup(_) do
    :application.set_env(:couch_normalizer_manager, :registry, :ets.new(:s2, [:ordered_set, {:keypos, 1}]))
  end


  test "try to lookup_next" do
    assert :couch_normalizer_manager.next_scenario("yo") == nil
  end

  test :lookup_next do
    assert CouchNormalizer.Registry.acquire("1-scenario", fn(x) -> x end) == true
    assert CouchNormalizer.Registry.acquire("2-scenario", fn(x) -> x end) == true

    {"2", "2-scenario", f}    = :couch_normalizer_manager.next_scenario("1")

    assert f.("hi there!")    == "hi there!"
    assert f.("how are you?") == "how are you?"
  end

end