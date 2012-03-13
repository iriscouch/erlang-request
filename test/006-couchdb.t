#! /usr/bin/env escript

-define(TIMEOUT, 3000).
-define(PORT, 5984).
-define(HOST, "http://localhost:" ++ integer_to_list(?PORT)).

-define(DOC_FREQ, 200).
-define(HB_FREQ, 150).

-import(request, [request/1, request/2, dot/2]).

main([]) -> ok
    , code:add_pathz("test")
    , code:add_pathz("ebin")
    , code:add_pathz("deps/ejson/ebin")

    , etap:plan(45)
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
    %, io:format("Body1: ~p\n", [Body1])
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
    , {Res4, _Body4} = request:put({[ {uri,Db} ]})
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
    , Testing_at = now()
    , spawn(fun() -> make_docs() end)
    , Url = ?HOST ++ "/erlang_request_test_db/_changes?feed=continuous&include_docs=true&heartbeat="++integer_to_list(?HB_FREQ)
    , {Res, Data} = request({[ {url,Url}, {onResponse,true} ]})
    , etap:isnt(Res, error, "Fetch changes feed")
    , etap:is(Res:statusCode(), 200, "Good reply on changes feed")

    , Response_at = now()
    , Change1 = ejson:decode(Data())
    , etap:is(dot(Change1, seq), 1, "Got first change, the test doc")
    , Change2 = ejson:decode(Data())
    , etap:is(dot(Change2, id), <<"_design/ereq">>, "Got second change, the ddoc")
    , etap_roughly(Response_at, 0, "Changes with existing docs came immediately")

    , Hb1 = Data()
    , etap:is(Hb1, <<"\n">>, "First heartbeat")

    , Change3 = ejson:decode(Data())
    , etap:is(dot(Change3, ".doc.count"), 1, "Got first doc")
    , Change3_at = etap_roughly(Testing_at, ?DOC_FREQ, "First change is 300ms since testing began")

    , Hb2 = Data()
    , etap:is(Hb2, <<"\n">>, "Second heartbeat")
    , Hb2_at = etap_roughly(Change3_at, ?HB_FREQ, "Second heartbeat after the first doc")

    , Change4 = ejson:decode(Data())
    , etap:is(dot(Change4, ".doc.count"), 2, "Got second doc")
    , Change4_at = etap_roughly(Hb2_at, ?DOC_FREQ - ?HB_FREQ, "Second change since the heartbeat")

    , Hb3 = Data()
    , etap:is(Hb3, <<"\n">>, "Third heartbeat")
    , etap_roughly(Change4_at, ?HB_FREQ, "Third heartbeat after the second doc")

    , Change5 = ejson:decode(Data())
    , etap:is(dot(Change5, ".doc.count"), 3, "Got third doc")
    , Change5_at = etap_roughly(Change4_at, ?DOC_FREQ, "Third doc since second doc")

    , Hb4 = Data()
    , etap:is(Hb4, <<"\n">>, "Fourth heartbeat")
    , etap_roughly(Change5_at, ?HB_FREQ, "Fourth heartbeat after the third doc")

    , Hb5 = Data()
    , etap:is(Hb5, <<"\n">>, "Fifth heartbeat")
    , etap_roughly(Change5_at, 2 * ?HB_FREQ, "Fifth heartbeat after the third doc")

    , etap:is(Res:destroy(), ok, "Close the changes feed")
    .

% Test that the elapsed time since a given timestamp is within 10ms of the estimate, or 10%, whichever is greater.
etap_roughly(Since, Estimate, Description) -> ok
    , Now = now()
    , Elapsed_ms = timer:now_diff(Now, Since) / 1000

    , Ten_pc_down = Estimate * 0.9
    , Ten_pc_up   = Estimate * 1.1
    , Ten_ms_down = Estimate - 10
    , Ten_ms_up   = Estimate + 10

    , {Low, High} = case Ten_ms_up > Ten_pc_up
        of true -> {Ten_ms_down, Ten_ms_up}
        ; false -> {Ten_pc_down, Ten_pc_up}
        end

    , Above_low_mark = Low < Elapsed_ms
    , Below_high_mark = Elapsed_ms < High

    , Desc = lists:flatten(io_lib:format("~s (~w ms)", [Description, Elapsed_ms]))
    , etap:ok(Above_low_mark andalso Below_high_mark, Desc)
    , Now
    .

make_docs() -> ok
    , make_docs(1)
    .

make_docs(Count) when Count > 3 -> ok
    , etap:is(Count, 4, "Finished making docs")
    ;

make_docs(Count) -> ok
    , timer:sleep(?DOC_FREQ)
    , Count_s = integer_to_list(Count)
    , Id = "doc_" ++ Count_s
    , Doc = {[ {'_id',list_to_binary(Id)}, {count,Count} ]}
    , {Res, _Body} = request:post({[ {url,?HOST++"/erlang_request_test_db"}, {json,Doc} ]})
    , etap:ok(Res =/= error andalso Res:statusCode() == 201, "Created document " ++ Count_s)
    , make_docs(Count+1)
    .

% vim: sts=4 sw=4 et
