use CouchNormalizer.Scenario

CouchNormalizer.Registry.acquire "1-example-scenario", fn(db, _doc_id, _rev, body) ->
  # 0. retrieves field value from the given document body.
  # `field/1`, `field/2` just another convenience for getting values from the body.
  if body["type"] == "user" do
    # 1. updates/improves document structure.
    update_field :field, String.upcase(body["field"])
    rename_field :old_name, :new_name

    # 2. removes unused/deprecated fields.
    remove_field  :unused_a
    remove_fields [:unused_b, :unused_c, :unused_d]

    # 3. creates new fields.
    create_field :string, "string"
    create_field :array,  ["hello", "world"]
    create_field :hash,   {[{"key", "value"}, {"key_1", "value_1"}]}
    create_field :integer, 10

    # 4. reads some value from external document.
    create_field :link, doc_field("ddoc", :link)
    # 4.1 reads some doc and value from external db.
    create_field :link1, doc_field("db", "ddoc", :link)
    # 4.2 reads external document once and cache it.
    # so, further method calls (during particular normalization session) returns cached value.
    create_field :link2, doc_field(db, "ddoc", :link, :cached!)

    if field(body, :no_longer_available) == true do
      # 5. removes current document (mark as _deleted = true).
      mark_as_deleted!
      # 5.1 removes some external document.
      remove_document!("db", field(:ticket_id))
    end

    # Finally, notifies the normalizer engine about changes which should be stored (update document).
    {:update, body}
  end
end
