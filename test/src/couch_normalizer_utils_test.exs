Code.require_file "../../test_helper.exs", __FILE__

defmodule CouchNormalizerUtilsTest do
  use ExUnit.Case, async: false

  def setup(_), do: CouchNormalizer.Registry.init


  test "tries to lookup_next" do
    ets = CouchNormalizer.Registry.to_ets
    assert :couch_normalizer_utils.next_scenario(ets, "yo") == nil
  end

  test :lookup_next do
    assert CouchNormalizer.Registry.acquire("1-scenario", fn(x) -> x end) == true
    assert CouchNormalizer.Registry.acquire("2-scenario", fn(x) -> x end) == true

    ets = CouchNormalizer.Registry.to_ets

    {"2", "2-scenario", f}    = :couch_normalizer_utils.next_scenario(ets, "1")

    assert f.("hi there!")    == "hi there!"
    assert f.("how are you?") == "how are you?"
  end

end