[![Build Status](https://secure.travis-ci.org/Zatvobor/couch_normalizer.png?branch=master "Build Status")](http://travis-ci.org/Zatvobor/couch_normalizer)

Works for CouchDB '1.2.x' versions

Couch Normalizer: a convenient way for you to alter your document database in a structured and organized manner.

We've been using a `Couch Normalizer` as a part of our data driven engineering for normalizing a large amount of documents obtained from multiple sources. Written both in Erlang and [Elixir](https://github.com/elixir-lang/elixir) and works well in production and has great performance.

The `Couch Normalizer` designed as a standard httpd handler for `Apache CouchDB` and uses a `Rails` approach such as `migrations`.

As a result, it allows to deploy migration scenarios and change a big amount of documents as fast as possible using a `CouchDB` internal functions such as `couch_db:open_doc/2`, `couch_db:update_doc/3` and so on.


Let's consider a definition DSL for [example](https://github.com/Zatvobor/couch_normalizer/blob/master/examples/1-example-scenario.exs):

```elixir
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
```

In short, a `CouchNormalizer.Registry.acquire/2` accepts scenario `title` and `callback` function with `db, _doc_id, _rev, body` arguments which will be applied for each document inside your `CouchDB` in context of particular `db`. If `callback` function returns `{:update, body}` statement then `body` should be stored into particular `db` as is. Actually, you can use your own code inside this function and do anything with passed context as you need.


So, send a POST to `/db/_normalize` resource and start the normalization process:

    curl -v -XPOST -H"Content-Type: application/json" http://127.0.0.1:5984/db/_normalize
    => {"ok":"Normalization process has been started (<0.174.0>)."}


Each normalized (already updated) documents have a special `rev_history_` field:

      "rev_history_" => {"title" => "1-example-scenario", "normpos" => 1}

Actually, `normpos` is an anchor which means that particular document already updated according to `1-example-scenario`.

Fast facts are:

* all normalization scenarios should be named as `"2-..."`, `"3-..."` (where prefix treats as `normpos`);
* a document which already has `"normpos"=>1` parameter should not be applied for `"1-example-scenario"` again;
* a document which already has `"normpos"=>1` parameter will be applied _just only_ for `"2-.."`, `"3-.."` scenarios once;
* a document should be updated when `callback` function returned _just only_ `{:update, body}` statement;


Check more advanced examples:

* [examples/2-acquire-missing-artists-fields.exs](https://github.com/Zatvobor/couch_normalizer/blob/master/examples/2-acquire-missing-artists-fields.exs)

* [examples/3-add-artist-uri-to-track-if-album-has-it.exs](https://github.com/Zatvobor/couch_normalizer/blob/master/examples/3-add-artist-uri-to-track-if-album-has-it.exs)

Scenario DSL API
----------------

A [CouchNormalizer.Scenario](https://github.com/Zatvobor/couch_normalizer/blob/master/lib/couch_normalizer/scenario.ex) module would be a good start point.


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
    ; '5' means num of workers
    couch_normalizer_manager={couch_normalizer_manager, start_link, [[{seed_labeled, "/path/to/scenarios", 5}]]}


That's it. Stay tuned!

If you want to contribute, feel free to open an Github issue or submit a pull request.


License
-------

`Couch Normalizer` source code is released under Apache 2 License.
Check [LICENSE](https://github.com/Zatvobor/couch_normalizer/blob/master/LICENSE) and [NOTICE](https://github.com/Zatvobor/couch_normalizer/blob/master/NOTICE) files for more details.
