-module(couch_doc_behavior).
-compile(export_all).


run(Scenario) ->
  spawn(fun() -> in_case(Scenario) end),
  started.


in_case(single_process) ->
  Seq = lists:seq(1, 30000),

  Try = fun(_) ->
    Doc = couch_normalizer_db:document_body(<<"seed_labeled_prod">>,  <<"medianet:album:10000053">>),
    couch_normalizer_db:update_doc(<<"seed_labeled_prod">>, Doc)
  end,

  lists:foreach(Try, Seq);

in_case(spawned_processes) ->
  Seq = lists:seq(1, 5000),

  Try = fun(_) ->
    spawn(fun() ->
      Doc = couch_normalizer_db:document_body(<<"seed_labeled_prod">>,  <<"medianet:album:10000053">>),
      couch_normalizer_db:update_doc(<<"seed_labeled_prod">>, Doc)
    end)
  end,

  lists:foreach(Try, Seq);

in_case(spawned_update_processes) ->
  Seq = lists:seq(1, 30000),

  Try = fun(_) ->
    Doc = couch_normalizer_db:document_body(<<"seed_labeled_prod">>,  <<"medianet:album:10000053">>),
    spawn(fun() ->
      couch_normalizer_db:update_doc(<<"seed_labeled_prod">>, Doc)
    end)
  end,

  lists:foreach(Try, Seq);

in_case(fin) ->
  fin.