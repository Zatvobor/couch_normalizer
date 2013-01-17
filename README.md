Couch Normalizer: A convenience for massive document migration
---------------------------------------------------------------

at Datahogs we're using `Couch Normalizer` (it's a result of our challenges) as a part of our data driven engineering to one of music startup.

`Couch Normalizer` designed as a standard Couch DB httpd handler (it means that you have a RESTfull interface for interoperability) and uses Rails db migration approach. Written both in Erlang and Elixir. Works well on production and has a great IO performance.

As a result, it allows a developer to deploy migration scripts (aka scenarios) and changes big amount of documents as fast as possible without HTTP overhead and certain 'delayed jobs'.


[Example Scenario DSL](https://github.com/datahogs/couch_normalizer/blob/master/examples/1-example-scenario.exs)
----------------------------------------------------------------------------------------------------------------

Imagine, you manage a big DB with multiple document type ('user', 'track', 'album', 'artist' and so on) and you have to improve/change structure for all 'user' documents once or just only for couple of recent documents.

Let's consider an example:

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

          # Finally, notifies the normalizer engine about changes which should be applied (it updates a document).
          {:update, body}
        end
      end

In short, `CouchNormalizer.Registry.acquire/2` accepts scenario `title` and `callback` function which will be applied for each document inside your DB. Whether `callback` returns `{:update, body}` where `body` is a new version of document, then `Couch Normalizer` will update this document.

Each normalized document has a special `rev_history_` field which contains recent normalization info:

      "rev_history_" => {"title" => "1-example-scenario", "normpos" => 1}

Actually, `normpos` is a anchor and means that some document meets some scenario. So, for further normalization only 'user' documents without `"normpos" => 1` will be processed and updated.
