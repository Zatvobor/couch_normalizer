-module(couch_normalizer_httpd_db).
%
% HTTP handlers for starting and canceling a normalization process.
%

-include("couch_db.hrl").
-export([handle_normalize_req/2]).

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,last_chunk/1,end_json_response/1,
    start_chunked_response/3, send_error/4]).



handle_normalize_req(#httpd{method='POST',path_parts=[_,_]}=Req, #db{name=DbName}=Db) ->
  ok = couch_db:check_is_admin(Db),
  couch_httpd:validate_ctype(Req, "application/json"),

  A = gen_server:call(couch_normalizer_manager, {normalize, DbName}),
  send_json(Req, 202, {[A]});

handle_normalize_req(#httpd{method='POST',path_parts=[_,_,<<"cancel">>]}=Req, #db{name=DbName}=Db) ->
  ok = couch_db:check_is_admin(Db),
  couch_httpd:validate_ctype(Req, "application/json"),

  A = gen_server:call(couch_normalizer_manager, {cancel, DbName}),
  send_json(Req, 202, {[A]});

handle_normalize_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").