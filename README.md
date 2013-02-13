[![Build Status](https://secure.travis-ci.org/datahogs/couch_normalizer.png?branch=master "Build Status")](http://travis-ci.org/datahogs/couch_normalizer)

Couch Normalizer: A convenience for massive document migration
---------------------------------------------------------------

at Datahogs we've been using a `Couch Normalizer` (it's a result of our challenges) as a part of our data driven engineering to one of music startup.

The `Couch Normalizer` designed as a standard Apache CouchDB httpd handler (it means that you got a RESTfull interface for interoperability) and uses Rails db migration approach. Written both in Erlang and [Elixir](https://github.com/elixir-lang/elixir). Works well on production and has a great IO performance.

As a result, it allows to deploy migration scripts (aka scenarios) and change big amount of documents as fast as possible (without HTTP overhead and some kind of 'delayed jobs') via internal CouchDB functions, such as `couch_db:open_doc/2`, `couch_db:update_doc/3` and so on.

[Scenario example](https://github.com/datahogs/couch_normalizer/blob/master/examples/1-example-scenario.exs)
----------------------------------------------------------------------------------------------------------------

Imagine, you manage a big DB which contains 'user', 'track', 'album', 'artist' documents and you have to improve/change structure for all 'user' documents once or just only for couple of recent (daily) documents as quick as possible, without network issues/latency, fallbacks and execution monitoring...

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

All you need to do, just to submit a POST request:

    curl -v -XPOST -H"Content-Type: application/json" http://127.0.0.1:5984/db/_normalize
    => {"ok":"Normalization process has been started (<0.174.0>)."}

As a result, a `Couch Normalizer` starts internal iterator for `db` database and tries to apply each document to `1-example-scenario` scenario.

In short, `CouchNormalizer.Registry.acquire/2` accepts scenario `title` and `callback` function which will be applied for each document inside your CouchDB. Whether `callback` returns `{:update, body}`, then `Couch Normalizer` will update this document immediately.

Each normalized document has a special `rev_history_` field which contains recent normalization info:

      "rev_history_" => {"title" => "1-example-scenario", "normpos" => 1}

Actually, `normpos` is a anchor and means that some document meets some scenario. So, for further normalization only 'user' documents without `"normpos" => 1` will be processed and updated.

All further migrations should be called as `2-...`, `3-...`. In case when `Couch Normalizer` processes a document which hasn't yet a `normpos`, then processing engine will try to apply a document form the `1-...` to `X-...`.


Check more advanced examples:

* [examples/2-acquire-missing-artists-fields.exs](https://github.com/datahogs/couch_normalizer/blob/master/examples/2-acquire-missing-artists-fields.exs)

* [examples/3-add-artist-uri-to-track-if-album-has-it.exs](https://github.com/datahogs/couch_normalizer/blob/master/examples/3-add-artist-uri-to-track-if-album-has-it.exs)


Scenario DSL API
----------------

A [CouchNormalizer.Scenario](https://github.com/datahogs/couch_normalizer/blob/master/lib/couch_normalizer/scenario.ex) module would be a good start point.


Normalization HTTP API
----------------------

Check the [couch_normalizer_httpd_db](https://github.com/datahogs/couch_normalizer/blob/master/src/couch_normalizer_httpd_db.erl) module for examples and documentation.


Installation Quickstart
-----------------------

After downloading, type:

    make setup              # get-deps compile test
    make get-couchdb-deps   # Optional: clone couch db 1.2.x git from apache repos if you want to use a CouchDB as dependency
    make setup-dev-couchdb  # Optional: install CouchDB development version, and you'll have a `deps/couchdb/utils/./run -i`

After passed tests, you will be ready for final configuration step:

put in `elixir` and `couch_normalizer` ebins to `couchdb` bash script

    ELIXIR_PA_OPTIONS="-pa /var/www/couch_normalizer/current/deps/elixir/lib/elixir/ebin"
    COUCH_NORMALIZER_PA_OPTIONS="-pa /var/www/couch_normalizer/current/ebin"
    ERL_START_OPTIONS="$ERL_OS_MON_OPTIONS -sasl errlog_type error +K true +A 4 $ELIXIR_PA_OPTIONS $COUCH_NORMALIZER_PA_OPTIONS"

configure CouchDB `local.ini` config

    [httpd_db_handlers]
    _normalize = {couch_normalizer_httpd_db, handle_normalize_req}

    [daemons]
    couch_normalizer_manager={couch_normalizer_manager, start_link, [[{seed_labeled, [{scenarios_path, "/path/to/scenarios"}, {num_workers, 5}]}]]}


That is it. Stay tuned!

If you want to contribute, feel free to open an Github issue or submit a pull request.


License
-------

`Couch Normalizer` source code is released under Apache 2 License.
Check [LICENSE](https://github.com/datahogs/couch_normalizer/blob/master/LICENSE) and [NOTICE](https://github.com/datahogs/couch_normalizer/blob/master/NOTICE) files for more details.
