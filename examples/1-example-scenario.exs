import CouchNormalizer.Scenario

CouchNormalizer.Scenario.define "1-example-scenario", fn(_db, _doc_id, _rev, body) ->

  # before_filter
  if field(body, :type) == "user" do
    # 1. update/improve document structure
    update_field    :field, String.downcase(field(body, :b))
    rename_field    :old_name, :new_name

    # 2. cleanup unused/deprecated fields
    remove_field    :unused_a
    remove_fields   [:unused_b, :unused_c, :unused_d]

    # 3. create fields
    create_field :string, "string"
    create_field :array,  ["hello", "world"]
    create_field :hash,   {[{"key", "value"}, {"key_1", "value_1"}]}
    create_field :number, 10

    {:update, body}
  end
end
