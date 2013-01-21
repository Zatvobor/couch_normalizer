Code.require_file "../../../../test_helper.exs", __FILE__

defmodule CouchNormalizer.Scenario.FieldMethodsTest do
  use ExUnit.Case, async: true

  use CouchNormalizer.Scenario

  @body HashDict.new [{"field", :v}, {"field_2", :v2}, {"field_3", :v3}]


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

    assert_equal([{"field_3", :v3}], body)
  end

  test :remove_fields do
    body = @body
    remove_fields [:unknown, :field, :field_2, :unknown]

    assert_equal([{"field_3", :v3}], body)
  end

  test :rename_field do
    expected = [{"field_2",:v2},{"field_3",:v3},{"new_field",:v}]
    body     = @body

    rename_field(:unknown, :a)
    rename_field(:field, :new_field)

    assert_equal(expected, body)
  end

  test :update_field do
    expected = [{"field",:updated},{"field_2",:v2},{"field_3",:v3}]
    body     = @body

    update_field(:unknown, :a)
    update_field(:field, :updated)

    assert_equal(expected, body)
  end

  test :create_field do
    expected = [{"field","new_value"},{"field_1",:new_value},{"field_2",["hello"]},{"field_3",1}]
    body     = HashDict.new

    create_field :field,   "new_value"
    create_field :field_1, :new_value
    create_field :field_2, ["hello"]
    create_field :field_3, 1

    assert_equal(expected, body)
  end



  defp assert_equal(list, dict) do
    assert list == HashDict.to_list(dict)
  end

end