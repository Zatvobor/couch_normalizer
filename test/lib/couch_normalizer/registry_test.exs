Code.require_file "../../../test_helper.exs", __FILE__

defmodule CouchNormalizer.RegistryTest do
  use ExUnit.Case, async: true

  def setup(_), do: CouchNormalizer.Registry.init


  test "tries to acquire with invalid title (without normpos)" do
    assert_raise RuntimeError, fn ->
      CouchNormalizer.Registry.acquire("scenario", fn(_x) -> nil end)
    end
  end


  test "acquire(title, scenario)" do
    assert CouchNormalizer.Registry.acquire("1-scenario", fn(_x) -> nil end) == true
    assert CouchNormalizer.Registry.acquire("10-scenario", fn(_x) -> nil end) == true

    registry = CouchNormalizer.Registry.to_ets

    [s] = :ets.lookup(registry, 1)

    assert :ets.member(registry, 1)  == true
    assert :ets.member(registry, 10) == true

    {1, "1-scenario", _} = s
  end


  test "acquire(title, scenario) multiple" do
    Enum.each ["9-scn", "22-scn", "10-scn", "3-scn", "2-scn", "1-scn"], CouchNormalizer.Registry.acquire(&1, fn(_x) -> nil end)
    registry = CouchNormalizer.Registry.to_ets

    assert :ets.first(registry)    == 1

    assert :ets.next(registry, 1)  == 2
    assert :ets.next(registry, 2)  == 3
    assert :ets.next(registry, 3)  == 9
    assert :ets.next(registry, 10) == 22

    assert :ets.last(registry)     == 22
  end

  test "tries to load a missing scenario" do
    assert_raise ArgumentError, fn() ->
      CouchNormalizer.Registry.load("test/missing-scenario.exs")
    end
  end

  @subject "examples/1-example-scenario.exs"

  test "loads a '#{@subject}' scenario" do
    CouchNormalizer.Registry.load @subject

    [{ normpos, title, fun }] = :ets.lookup(CouchNormalizer.Registry.to_ets, 1)

    assert normpos == 1
    assert title == "1-example-scenario"
    assert is_function(fun)
  end

end