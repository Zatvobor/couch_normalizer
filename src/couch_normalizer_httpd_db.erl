-module(couch_normalizer_httpd_db).
%
% HTTP handlers for managing a normalization process.
%
-include("couch_db.hrl").
-export([handle_normalize_req/2]).

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,last_chunk/1,end_json_response/1,
    start_chunked_response/3, send_error/4]).


% Gets a normalization process execution status.
%   curl -v http://127.0.0.1:5984/_active_tasks
%   => [{"pid":"<0.174.0>","continue":false,"db":"db","docs_conflicted":0,"docs_deleted":0,"docs_normalized":0,"docs_read":3000,"finished_on":1358513508,"num_workers":5,"started_on":1358513508,"type":"normalization","updated_on":1358513508}]


% Starts a normalization process.
%   curl -v -XPOST -H"Content-Type: application/json" http://127.0.0.1:5984/db/_normalize
%   => {"ok":"Normalization process has been started (<0.174.0>)."}
handle_normalize_req(#httpd{method='POST',path_parts=[_,_]}=Req, #db{name=DbName}=Db) ->
  ok = couch_db:check_is_admin(Db),
  couch_httpd:validate_ctype(Req, "application/json"),

  A = gen_server:call(couch_normalizer_manager, {normalize, DbName}),
  send_json(Req, 202, {[A]});

% Stops a normalization process.
%   curl -v -XPOST -H"Content-Type: application/json" http://127.0.0.1:5984/db/_normalize/cancel
%   => {"ok":"terminated"}
handle_normalize_req(#httpd{method='POST',path_parts=[_,_,<<"cancel">>]}=Req, #db{name=DbName}=Db) ->
  ok = couch_db:check_is_admin(Db),
  couch_httpd:validate_ctype(Req, "application/json"),

  A = gen_server:call(couch_normalizer_manager, {cancel, DbName}),
  send_json(Req, 202, {[A]});

handle_normalize_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").