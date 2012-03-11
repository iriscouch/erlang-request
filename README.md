# Request: The easiest Erlang HTTP library you'll ever see

Erlang Request is a port of Mikeal Rogers's ubiquitous and excellent [request][req] package to Erlang.

# Examples

Fetch a resource asynchronously.

Fetch a resource the Erlangy synchronous way:

```erlang
case request:req("http://example.com/some/resource.txt")
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
Pid = request:req("http://example.com/some/resource.txt", fun
    (error, Error) ->
        io:format("Got an error: ~p\n", [Error])
    ; (Response, Body) ->
        io:format("Got a ~w, type ~s\n", [Response:statusCode(), Response:headers('content-type')]),
        io:format("~s\n", [Body])
    end),

io:format("Request is in-flight as ~p\n", [Pid])
```

You can also get a more familiar access to Request's main function.

```erlang
Request = request:api(), % Home, sweet home

{Response, Body} = Request("http://example.com/")
```

Or asynchronously

```erlang
Request = request:api(async), % Home, sweet home

Request("http://example.com/some/resource.txt", fun
    (Response, Body) ->
        io:format("This feels nice: " ++ Body)
    end)
```

## Details

More advanced usage works like JavaScript request. The first parameter is a JSON-style object.
Send a resource:

```erlang
request:put({[{uri,'/some/resource.xml'}, {body,'<foo><bar/></foo>'}]}, fun
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
Request([{method,'POST'}, {url,"/db"}, {json, {[{relaxed,true}]}}], fun on_response/2)
```

## License

Erlang Request is licensed under the Apache 2.0 license.

[req]: https://github.com/mikeal/request
