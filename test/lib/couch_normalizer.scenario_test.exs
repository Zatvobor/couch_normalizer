Code.require_file "../../test_helper.exs", __FILE__

defmodule CouchNormalizer.ScenarioTest do
  use ExUnit.Case, async: true

  use CouchNormalizer.Scenario

  @fixture [{"field", :v}, {"field_2", :v2}, {"field_3", :v3}]


  test "unknown field" do
    assert field(@fixture, :unknown) == :nil
    assert field(@fixture, :unknown) == nil
  end

  test :field do
    assert field(@fixture, :field) == :v
  end


  test :remove_field do
    body = @fixture

    remove_field(:unknown)
    remove_field(:field)
    remove_field("field_2")

    assert [{"field_3", :v3}] == body
  end

  test :remove_fields do
    body = @fixture

    remove_fields [:unknown, :unknown]
    remove_fields([:field, :field_2])

    assert [{"field_3", :v3}] == body
  end

  test "remove_fields when one field doesn't exist" do
    body = @fixture

    remove_fields([:field, :unknown, :field_2])
    assert [{"field_3", :v3}] == body
  end

  test :rename_field do
    body = @fixture

    rename_field(:unknown, :a)
    rename_field(:field, :new_field)

    assert [{"new_field", :v}, {"field_2", :v2}, {"field_3", :v3}] == body
  end

  test :update_field do
    body = @fixture

    update_field(:unknown, :a)
    update_field(:field, :updated)

    assert body == [{"field", :updated}, {"field_2", :v2}, {"field_3", :v3}]
  end

  test :create_field do
    body = []

    create_field :field,   "new_value"
    create_field :field_1, :new_value
    create_field :field_2, ["hello"]
    create_field :field_3, 1

    assert body == [{"field", "new_value"}, {"field_1", :new_value}, {"field_2", ["hello"]}, {"field_3", 1}]
  end


  test :mark_as_deleted! do
    body = @fixture

    mark_as_deleted!

    assert [{"field", :v}, {"field_2", :v2}, {"field_3", :v3}, {"_deleted", :true}] == body
  end

end