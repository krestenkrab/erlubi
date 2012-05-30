# Erlang Visualizer using Ubigraph

This is a simple visualizer for erlang systems using Ubigraph.

To use,

- Download the Ubigraph server from [ubietylab.net](http://ubietylab.net/ubigraph/content/Downloads/index.php).  Unpack it and just run the command line tool `bin/ubigraph_server`.  A black window will appear.
- Grab a copy of `erlubi` from github `git clone git://github.com/krestenkrab/erlubi.git`
- Compile `erlubi` using `./rebar get-deps; ./rebar compile`
- Run erlang with `-pa erlubi/ebin -pa erlubi/deps/xmlrpc/ebin` on the command line
- Launch the visualizer/tracer using `erlubi_tracer:run()` inside your erlang shell.

Now `erlubi_tracer` will first build a structure corresponding to present processes in your VM, and then install erlang trace hooks to receive notifications of new Erlang processes and links being created.

Enjoy!

## Using `erlubi`

 You can also use `erlubi` for your own visualizations in Erlang. 

- Run `erlubi:start()`
- Create vertex: `{ok, Vertex} = erlubi:vertex()`
- Create edge: `{ok, Edge} = erlubi:edge(Vertex1, Vertex2)`
- Set attributes:  `Vertex:shape(sphere), Edge:stroke(dashed)`
- Clear the graph space: `erlubi:clear()`

See `src/erlubi_edge.erl` and `src/erlubi_vertex.erl` for more attributes to set.


## Contribute

The code is available under APACHE LICENSE, you're free to clone, fix and add features.
    