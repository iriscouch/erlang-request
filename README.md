# Request: The easiest Erlang HTTP library you'll ever see

Erlang Request is a port of Mikeal Rogers's ubiquitous and excellent [request][req] package to Erlang.

# Examples

Fetch a resource the Erlangy synchronous way:

```erlang
-import(request, [request/1]).

case request("http://example.com/some/resource.txt")
    of {error, Error} ->
        io:format("Got an error: ~p\n", [Error])
    ; {Response, Body} ->
        io:format("Got ~w, headers: ~p\n", [Response:statusCode(), Response.headers()]),
        io:format("Body: ~p\n", [Body]) % or Response:body()
    end,

io:format("Request complete\n")
```

Or fetch a resource the Javascripty asynchronous way:

```erlang
Pid = request("http://example.com/some/resource.txt", fun
    (error, Error) ->
        io:format("Got an error: ~p\n", [Error])
    ; (Response, Body) ->
        io:format("Got a ~w, type ~s\n", [Response:statusCode(), Response:headers('content-type')]),
        io:format("~s\n", [Body])
    end),

io:format("Request is in-flight as ~p\n", [Pid])
```

## Details

More advanced usage works like JavaScript request. The first parameter is an EJSON object.
Send a resource:

```erlang
request:put({[ {uri,'/some/resource.xml'}, {body,"<foo><bar/></foo>"} ]}, fun
    (error, Er) ->
        io:format("XML PUT failed: ~p\n", [Er])
    ; (Res, Body) ->
        io:format("Stored the XML; HTTP status was ~w\n", [Res:statusCode()])
    end)
```

To work with JSON, set `options.json` to `true`. Request will set the `Content-Type` and `Accept` headers, and handle parsing and serialization to and from mochijson.

```erlang
request:post({[{url,"/db"}, {body,"{\"relaxed\":true}"}, {json,true}]}, fun on_response/2)

on_response(error, Er) ->
    throw(Er);

on_response(Res, Body) ->
    {[{id, Id}]} = Body,
    io:format("Server ok, id = ~w\n", [Id]).
```

Or, use this shorthand version (pass data into the `json` option directly):

```erlang
request({[ {method,'POST'}, {url,"/db"}, {json,{[ {relaxed,true} ]}} ]}, fun on_response/2)
```

## Streaming response

Like Node.js *request*, you can be called back immediately when the response (status and headers) arrives. Next, run the given function to receive body data.

```erlang
Url = "http://example.iriscouch.com/db/_changes?feed=continuous",
{Response, Get_body} = request({[ {url,Url}, {onResponse,true} ]})
case Response:statusCode() of
    200 -> stream_body(1, Get_body);
    _   -> io:format("Couch said no\n")
end

stream_body(Line_number, Get_body) ->
    case Get_body() of
        'end' ->
            io:format("End of body.\n");
        Data ->
            io:format("Chunk ~w: ~s\n", [Line_number, Data]),
            stream_body(Line_number + 1, Get_body)
    end.
```

## License

Erlang Request is licensed under the Apache 2.0 license.

[req]: https://github.com/mikeal/request
