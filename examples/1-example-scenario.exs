import CouchNormalizer.Scenario

CouchNormalizer.Scenario.define "1-example-scenario", fn(db, _doc_id, _rev, body) ->

  # before filter hook
  if field(body, :type) == "user" do

    # 1. updates/improves document structure
    update_field    :field, String.downcase(field(body, :b))
    rename_field    :old_name, :new_name

    # 2. cleanups unused/deprecated fields
    remove_field    :unused_a
    remove_fields   [:unused_b, :unused_c, :unused_d]

    # 3. creates fields
    create_field :string, "string"
    create_field :array,  ["hello", "world"]
    create_field :hash,   {[{"key", "value"}, {"key_1", "value_1"}]}
    create_field :number, 10

    # 4. read another document
    create_field :link, doc_field(db, "id", :link)

    # 5. removes current document (mark as _deleted = true)
    if field(body, :no_longer_available) == true do
      mark_as_deleted!

      # 6. removes some document
      remove_document!(db, field(body, :ticket_id))
    end

    # notifies the normalizer engine about changes which should be saved.
    {:update, body}
  end
end
