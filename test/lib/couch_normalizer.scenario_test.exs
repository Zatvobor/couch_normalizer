Code.require_file "../../test_helper.exs", __FILE__

defmodule CouchNormalizer.ScenarioTest do
  use ExUnit.Case, async: true


  import CouchNormalizer.Scenario

  @fixture [{"field", :v}, {"field_2", :v2}, {"field_3", :v3}]


  test :field do
    assert field(@fixture, :unknown) == :nil
    assert field(@fixture, :field) == :v
  end


  test :remove_field do
    body = @fixture

    remove_field(:unknown)
    assert @fixture == body

    remove_field(:field)
    assert [{"field_2", :v2}, {"field_3", :v3}] == body

    remove_field("field_2")
    assert [{"field_3", :v3}] == body
  end

  test :remove_fields do
    body = @fixture

    remove_fields [:unknown, :unknown]
    assert @fixture == body

    remove_fields([:field, :field_2])
    assert [{"field_3", :v3}] == body
  end

  test :rename_field do
    body = @fixture

    rename_field(:unknown, :a)
    assert @fixture == body

    rename_field(:field, :new_field)
    assert [{"new_field", :v}, {"field_2", :v2}, {"field_3", :v3}] == body
  end

  test :update_field do
    body = @fixture

    update_field(:unknown, :a)
    assert @fixture == body

    update_field(:field, :updated)
    assert body == [{"field", :updated}, {"field_2", :v2}, {"field_3", :v3}]
  end

end