-module(erlubi_util).

-compile(export_all).

to_string(S) when is_list(S)    -> S;
to_string(true)                 -> "true";
to_string(false)                -> "false";
to_string(A) when is_atom(A)    -> atom_to_list(A);
to_string(I) when is_integer(I) -> integer_to_list(I);
to_string(F) when is_float(F)   -> lists:flatten(io_lib:format("~f", [F]));
to_string(Other)                -> lists:flatten(io_lib:format("~P", [Other, 10])).



color_string(white) ->
    "#ffffff";
color_string(black) ->
    "#000000";
color_string({R,G,B}) ->
    lists:flatten([$#, to_hex(R), to_hex(G), to_hex(B)]);
color_string(Color) ->
    {R,G,B,_} = egd:color(Color),
    RInt = erlang:round(255 * R),
    GInt = erlang:round(255 * G),
    BInt = erlang:round(255 * B),
    color_string({RInt,GInt,BInt}).


to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a+(N-10).


