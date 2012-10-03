import CouchNormalizer.Scenario

CouchNormalizer.Scenario.define "1-test-scenario", fn(_db, _doc_id, _rev, body) ->

  # 1. update/improve document structure
  update_field    :field, String.downcase(field(body, :b))
  rename_field    :old_name, :new_name

  # 2. cleanup unused/deprecated fields
  remove_field    :unused_a
  remove_field    :unused_b

  {:update, body}
end
