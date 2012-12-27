Code.require_file "../../../../test_helper.exs", __FILE__

defmodule CouchNormalizer.Scenario.DocMethodsTest do
  use ExUnit.Case, async: true

  use CouchNormalizer.Scenario

  defmodule StubbedCouchDb do
    def body, do: [{"field", "value"}, {"field_2", "value"}]
    def document_body(_db, _id), do: body
    def update_doc(_body), do: :done!
  end

  CouchNormalizer.Scenario.under_test(StubbedCouchDb)
  @body StubbedCouchDb.body


  test "tries to get some document from current db" do
    db = "db"
    assert doc("ddoc") == @body
  end

  test "tries to get some document" do
    assert doc("db", "ddoc") == @body
  end

  test "tries to get current doc_field" do
    db = "db"
    assert doc_field("ddoc", :unknown) == nil
  end

  test "gets current doc_field" do
    db = "db"
    assert doc_field("ddoc", :field_2) == "value"
  end

  test "tries to get doc_field" do
    assert doc_field("db", "ddoc", :unknown) == nil
  end

  test "gets doc_field" do
    assert doc_field("db", "ddoc", :field) == "value"
  end

  test "removes some document from current db" do
    db = "db"
    assert remove_document!("ddoc") == :done!
  end

  test "removes some document" do
    assert remove_document!("db", "ddoc") == :done!
  end


  test :mark_as_deleted! do
    body = @body
    mark_as_deleted!

    assert @body ++ [{"_deleted", :true}] == body
  end

end