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




