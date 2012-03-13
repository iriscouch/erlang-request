#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 5984).
-define(HOST, "http://localhost:" ++ integer_to_list(?PORT)).

-import(request, [request/1, request/2, dot/2]).

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")
    , code:add_pathz("deps/ejson/ebin")

    , etap:plan(8)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_basics()
    , test_views()
    , test_changes()
    .

test_basics() -> ok
    , {Res1, _Body1} = request:get(?HOST ++ "/_bad_name")
    , etap:isnt(Res1, error, "CouchDB response")
    , etap:is(Res1:statusCode(), 400, "CouchDB doesn't like _bad_name")

    , {Res2, Body2} = request({[ {uri,?HOST}, {json,true} ]})
    , etap:isnt(Res2, error, "CouchDB welcome message")
    , etap:is(Res2:statusCode(), 200, "CouchDB returned 200 to /")
    , etap:is(dot(Body2, ".couchdb"), <<"Welcome">>, "CouchDB says welcome in JSON")

    , {Res3, Body3} = request({[ {uri,?HOST++"/_bad"}, {json,true} ]})
    , etap:isnt(Res3, error, "CouchDB responds to /_bad")
    , etap:is(Res3:statusCode(), 400, "CouchDB rejects /_bad with a 400")
    , etap:is(dot(Body3, ".error"), <<"illegal_database_name">>, "CouchDB said /_bad is a bad name")
    .

test_views() -> ok
    .

test_changes() -> ok
    .

% vim: sts=4 sw=4 et
