%% ----------------------------------------------------------------------------
%%
%% erlubi: Erlang Process Visualizer
%%
%% Copyright 2012 (c) Trifork A/S.  All Rights Reserved.
%% http://trifork.com/ info@trifork.com
%%
%% This file is provided to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
%% License for the specific language governing permissions and limitations
%% under the License.
%%
%% ----------------------------------------------------------------------------

-module(erlubi_vertex).

-export([new/1, id/1, color/2, shape/2, shapedetail/2, label/2, size/2, fontfamily/2,
        fontcolor/2, fontsize/2, visible/2, remove/1]).

new(ID) ->
    {?MODULE, [ID]}.

id({?MODULE, [ID]}) ->
    ID.

remove({?MODULE, [ID]}) ->
    erlubi:callv('ubigraph.remove_vertex', [ID]).

set(Att, Value, {?MODULE, [ID]}) ->
    erlubi:callv('ubigraph.set_vertex_attribute',
                 [ID,
                  erlubi_util:to_string(Att),
                  erlubi_util:to_string(Value)]).

color(Color, {?MODULE, [_ID]}=THIS) ->
    set("color", erlubi_util:color_string(Color), THIS).

shape(cone, {?MODULE, [_ID]}=THIS) ->
    set("shape", "cone", THIS);
shape(cube, {?MODULE, [_ID]}=THIS) ->
    set("shape", "cube", THIS);
shape(dodecahedron, {?MODULE, [_ID]}=THIS) ->
    set("shape", "dodecahedron", THIS);
shape(icosahedron, {?MODULE, [_ID]}=THIS) ->
    set("shape", "icosahedron", THIS);
shape(octahedron, {?MODULE, [_ID]}=THIS) ->
    set("shape", "octahedron", THIS);
shape(sphere, {?MODULE, [_ID]}=THIS) ->
    set("shape", "sphere", THIS);
shape(torus, {?MODULE, [_ID]}=THIS) ->
    set("shape", "torus", THIS);
shape(none, {?MODULE, [_ID]}=THIS) ->
    set("shape", "none", THIS).

shapedetail(N, {?MODULE, [_ID]}=THIS) when N >= 0 ->
    set("shapedetail", N, THIS).

label(Text, {?MODULE, [_ID]}=THIS) when is_list(Text) ->
    set("label", Text, THIS);
label(Any, {?MODULE, [_ID]}=THIS) ->
    set("label", Any, THIS).

size(Size, {?MODULE, [_ID]}=THIS) when is_number(Size) ->
    set("size", Size, THIS).

fontfamily(times, {?MODULE, [_ID]}=THIS) ->
    set("fontfamily", "Times Roman", THIS);
fontfamily(helvetica, {?MODULE, [_ID]}=THIS) ->
    set("fontfamily", "Helvetica", THIS).


fontcolor(Color, {?MODULE, [_ID]}=THIS) ->
    set("fontcolor", erlubi_util:color_string(Color), THIS).


fontsize(N, {?MODULE, [_ID]}=THIS) when is_integer(N) ->
    set("fontsize", N, THIS).


visible(true, {?MODULE, [_ID]}=THIS) ->
    set("visible", "true", THIS);
visible(false, {?MODULE, [_ID]}=THIS) ->
    set("visible", "false", THIS).



