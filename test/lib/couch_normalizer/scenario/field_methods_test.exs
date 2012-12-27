Code.require_file "../../../../test_helper.exs", __FILE__

defmodule CouchNormalizer.Scenario.FieldMethodsTest do
  use ExUnit.Case, async: true

  use CouchNormalizer.Scenario

  @body [{"field", :v}, {"field_2", :v2}, {"field_3", :v3}]


  test "tries to get undefined field" do
    assert field(@body, :undefined) == :nil
  end

  test "tries to get field for current document" do
    body = @body
    assert field(:field) == :v
  end

  test :field do
    assert field(@body, :field) == :v
  end


  test :remove_field do
    body = @body

    remove_field(:unknown)
    remove_field(:field)
    remove_field("field_2")

    assert [{"field_3", :v3}] == body
  end

  test :remove_fields do
    body = @body
    remove_fields [:unknown, :field, :field_2, :unknown]

    assert [{"field_3", :v3}] == body
  end

  test :rename_field do
    body = @body

    rename_field(:unknown, :a)
    rename_field(:field, :new_field)

    assert [{"new_field", :v}, {"field_2", :v2}, {"field_3", :v3}] == body
  end

  test :update_field do
    body = @body

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

end