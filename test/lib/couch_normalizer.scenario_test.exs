Code.require_file "../../test_helper.exs", __FILE__

defmodule CouchNormalizer.ScenarioTest do
  use ExUnit.Case, async: true


  import CouchNormalizer.Scenario

  @fixture [{"field", :v}, {"field_2", :v2}, {"field_3", :v3}]


  test :field do
    assert field(@fixture, :unknown) == nil
    assert field(@fixture, :field) == :v
  end


  test :remove_field do
    body = @fixture

    remove_field(:unknown)
    assert body == @fixture

    remove_field(:field)
    assert body == [{"field_2", :v2}, {"field_3", :v3}]

    remove_field("field_2")
    assert body == [{"field_3", :v3}]
  end

  test :remove_fields do
    body = @fixture

    # remove_fields([:unknown, :unknown])
    # assert body == @fixture

    remove_fields([:field_2, :field])
    assert body == [{"field_3", :v3}]
  end

  test :rename_field do
    body = @fixture

    rename_field(:unknown, :a)
    assert body == @fixture

    rename_field(:field, :new_field)
    assert body == [{"new_field", :v}, {"field_2", :v2}, {"field_3", :v3}]
  end

  test :update_field do
    body = @fixture

    update_field(:unknown, :a)
    assert body == @fixture

    update_field(:field, :updated)
    assert body == [{"field", :updated}, {"field_2", :v2}, {"field_3", :v3}]
  end

end