Code.require_file "../../../test_helper.exs", __FILE__

defmodule CouchNormalizer.ScenarioTest do
  use ExUnit.Case, async: true

  test "calls scenario functions which return nil" do
    scenario_fun = fn(_db_name, _id, _rev, _body) -> nil end

    what_we_should_do = CouchNormalizer.Scenario.call(scenario_fun, {"db", "ddoc", "rev", []})
    assert what_we_should_do == nil
  end

  test "calls scenario functions which should return {:update, body} instruction" do
    scenario_fun = fn(_db_name, _id, _rev, body) -> {:update, body} end

    { :update, body } = CouchNormalizer.Scenario.call(scenario_fun, {"db", "ddoc", "rev", [{"_id", "ddoc"}]})
    body = HashDict.to_list body
    assert body == [{"_id", "ddoc"}]
  end
end