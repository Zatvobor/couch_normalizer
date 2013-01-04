Code.require_file "../../test_helper.exs", __FILE__

defmodule CouchNormalizerUtilsTest do
  use ExUnit.Case, async: false

  def setup(_), do: CouchNormalizer.Registry.init


  test "tries to lookup next scenario for non numerical normpos" do
    assert_raise ArgumentError, fn ->
      :couch_normalizer_utils.next_scenario(CouchNormalizer.Registry.to_ets, "yo")
    end
  end

  test :lookup_next do
    assert CouchNormalizer.Registry.acquire("1-scenario", fn(x) -> x end) == true
    assert CouchNormalizer.Registry.acquire("2-scenario", fn(x) -> x end) == true
    assert CouchNormalizer.Registry.acquire("3-scenario", fn(x) -> x end) == true

    ets = CouchNormalizer.Registry.to_ets

    {1, "1-scenario", _} = :couch_normalizer_utils.next_scenario(ets, '0')
    {2, "2-scenario", _} = :couch_normalizer_utils.next_scenario(ets, "1")
    {3, "3-scenario", repeater} = :couch_normalizer_utils.next_scenario(ets, 2)

    assert repeater.("hi there!") == "hi there!"
    assert repeater.("how are you?") == "how are you?"
  end


  test "increases current normpos" do
    assert :couch_normalizer_utils.increase_current("10") == 11
    assert :couch_normalizer_utils.increase_current(18) == 19
  end

end