Code.require_file "../test_helper.exs", __FILE__


defmodule CouchNormalizer.ExampleScenarioTest do
  use ExUnit.Case, async: false

  defmodule StubbedCouchDb do
    def body, do: [ {"link", "example.com"} ]
    def document_body(_db, _id), do: body
    def update_doc(_body), do: :done!
  end

  CouchNormalizer.Scenario.under_test(StubbedCouchDb)

  @subject "examples/1-example-scenario.exs"

  def setup(_) do
    CouchNormalizer.Registry.init
    CouchNormalizer.Registry.load(@subject)
  end

  def subject_fun() do
    [{ _, _, subject }] = :ets.lookup(CouchNormalizer.Registry.to_ets, 1)
    subject
  end


  test "does load the #{@subject} scenario" do
    assert is_function(subject_fun)
  end

  test "tries to call with improper context" do
    assert CouchNormalizer.Scenario.call(subject_fun, {"db", "ddoc", "rev", [{"type", "track"}]}) == nil
  end

  test "updates some 'user' document" do
    # should be removed
    removed_fields = [ {"unused_a", "a"}, {"unused_b", "b"}, {"unused_c", "c"}, {"unused_d", "d"}, {"old_name", "name"} ]
    # should be updated
    updated_fields = [ {"field", "UPCASE"}, {"new_name", "name"} ]
    # should be created
    created_fields = [ {"string", "string"}, {"array",  ["hello", "world"]}, {"hash", {[{"key", "value"}, {"key_1", "value_1"}]}}, {"integer", 10} ]
    # should be created
    # created_external_fields = [ {"link", "example.com"}, {"link1", "example.com"}, {"link2", "example.com"} ]

    # should be normalized by @subject scenario
    document = [ {"type", "user"} ] ++ removed_fields ++ updated_fields ++ created_fields
    { :update, body } = CouchNormalizer.Scenario.call(subject_fun, {"db", "ddoc", "rev", document})
    body = HashDict.new body

    # should be removed
    assert_fields_should_be removed_fields, fn({k,_v}) -> body[k] == :nil end
    # should be updated
    assert_fields_should_be updated_fields, fn({k,v})  -> body[k] == v end
    # should be created
    assert_fields_should_be created_fields, fn({k,v})  -> body[k] == v end
    # should be created
    # assert_fields_should_be created_external_fields, fn({k,v})  -> body[k] == v end
  end


  def assert_fields_should_be(fields, map_fun) do
    mapped = Enum.map fields, map_fun
    Enum.each mapped, fn(s) -> assert(s) end
  end
end