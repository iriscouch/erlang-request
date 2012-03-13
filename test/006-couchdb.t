#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 5984).
-define(HOST, "http://localhost:" ++ integer_to_list(?PORT)).

-import(request, [request/1, request/2, dot/2]).

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")
    , code:add_pathz("deps/ejson/ebin")

    , etap:plan(20)
    , test()
    , etap:end_tests()
    .

test() -> ok
    , test_basics()
    , test_views()
    , test_changes()
    .

test_basics() -> ok
    , {Res1, Body1} = request:get(?HOST ++ "/_bad_name")
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

    , request:delete(?HOST ++ "/erlang_request_test_db")

    , Db = ?HOST ++ "/erlang_request_test_db"
    , {Res4, Body4} = request:put({[ {uri,Db} ]})
    , etap:isnt(Res4, error, "PUT " ++ Db)
    , etap:ok(Res4:statusCode() == 201 orelse Res4:statusCode() == 412, "Created test db")

    , Pid = list_to_integer(os:getpid())
    , {Res5, Body5} = request:post({[ {uri,Db}, {json,{[ {pid,Pid} ]}} ]})
    , etap:isnt(Res5, error, "Create document")
    , etap:is(Res5:statusCode(), 201, "201 for document creation")
    , etap:is(dot(Body5, ok), true, "Document creation returns ok:true")

    , Id = dot(Body5, id)
    , Doc_url = Db ++ "/" ++ binary_to_list(Id)
    , {Res6, Body6} = request({[ {url,Doc_url}, {json,true} ]})
    , etap:isnt(Res6, error, "Fetch the document back from CouchDB")
    , etap:is(dot(Body6, pid), Pid, "Got document contents back")
    .

test_views() -> ok
    , Pid = list_to_integer(os:getpid())
    , Id = "_design/ereq"
    , Map = <<"function(doc) { if(doc.pid) emit(doc.pid, doc) }">>
    , DDoc = {[ {'_id', list_to_binary(Id)}
              , {views,
                    {[ {pids, {[ {map, Map} ]}}
                    ]}
                }
             ]}

    , Db = ?HOST ++ "/erlang_request_test_db"
    , DDoc_url = Db ++ "/" ++ Id
    , {Res1, _Body1} = request:put({[ {url,DDoc_url}, {json,DDoc} ]})
    , etap:isnt(Res1, error, "POST design document")
    , etap:is(Res1:statusCode(), 201, "Successful ddoc post")

    , View_url = DDoc_url ++ "/_view/pids"
    , {Res2, Body2} = request({[ {url,View_url}, {json,true} ]})
    , etap:isnt(Res2, error, "Get the view back")
    , etap:is(dot(Body2, ".rows.0.key"), Pid, "Document is in first view row")
    , etap:is(dot(Body2, ".rows.0.value.pid"), Pid, "Document body is in the view value")
    .

test_changes() -> ok
    .

% vim: sts=4 sw=4 et
