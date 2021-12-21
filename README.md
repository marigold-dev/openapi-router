Openapi for Ocaml
=================

This library provides basic types for parsing/serializing openapi
specifications and a nearly drop-in replacement for `Opium.App` that
registers endpoints in a specification and serves `openapi.json` and
`docs` pages based on the `fastapi` implementation.

Usage
=====

Basic usage should work equivalently to `Opium.App`. So instead of
```
let open Opium in
App.empty
|> App.get "/path/:foo" (fun h -> Response.of_plain_text "hello world")
|> App.run_command
```

Just do:
```
let open Opium in
let module App = Openapi.App in
App.empty
|> App.get "/path/:foo" (fun h -> Response.of_plain_text "hello world")
|> App.run_command
```

The endpoint creation methods provided by `Openapi.Opium.App` accept
additional optional arguments to provide specification information for
to document the endpoint. Check the type signatures of these methods
for more information.

Demo App
========
This library includes a demo app in `bin/demo` (that won't be installed
by `opam`). To see the generated spec/documentation in action, run:
```
$ dune build
$ ./_build/default/bin/demo.exe
```
Then browse to http://localhost:8888/docs



