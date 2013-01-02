Code.require_file "../../../../test_helper.exs", __FILE__

defmodule CouchNormalizer.Scenario.CachedMethodsTest do
  use ExUnit.Case, async: true

  use CouchNormalizer.Scenario

  defmodule StubbedCouchDb do
    def body, do: HashDict.new [{"field", "value"}, {"field_2", "value"}]
    def document_body(_db, _id), do: body
    def update_doc(_body), do: :done!
  end

  CouchNormalizer.Scenario.under_test(StubbedCouchDb)
  @body StubbedCouchDb.body


  test "asserts empty process dictionary" do
    assert cached("db-ddoc") == :nil
  end

  test "gets doc as a cached resource" do
    assert doc("db", "ddoc", :cached!) == @body
    assert cached("db-ddoc") == @body
  end

  test "gets doc_field as a cached resource" do
    assert doc_field("db", "ddoc", "field", :cached!) == "value"
    assert cached("db-ddoc-field") == "value"
  end

end