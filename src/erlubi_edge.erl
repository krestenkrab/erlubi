-module(erlubi_edge, [ID]).

-export([id/0]).

-export([remove/0, arrow/1, arrow_position/1, spline/1, stroke/1, visible/1]).

id() ->
    ID.

remove() ->
    erlubi:callv('ubigraph.remove_edge', [ID]).


set(Att,Value) ->
    erlubi:callv('ubigraph.set_edge_attribute',
                 [ID,
                  erlubi_util:to_string(Att),
                  erlubi_util:to_string(Value)]).


arrow(Boolean) when is_boolean(Boolean) ->
    set("arrow", Boolean).

arrow_position(Where) when 0.0 =< Where, Where =< 1.0 ->
    set("arrow_position", lists:flatten( io_lib:format("~f", [Where]))).

spline(Boolean) when is_boolean(Boolean) ->
    set("spline", Boolean).

stroke(solid) ->
    set("stroke", "solid");
stroke(dashed) ->
    set("stroke", "dashed");
stroke(none) ->
    set("stroke", "none").

visible(Boolean) when is_boolean(Boolean) ->
    set("visible", Boolean).




