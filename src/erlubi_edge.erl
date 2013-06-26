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

-module(erlubi_edge).

-export([new/1, id/1]).

-export([remove/1, arrow/2, arrow_position/2, spline/2, stroke/2, visible/2]).

new(ID) ->
    {?MODULE, [ID]}.

id({?MODULE, [ID]}) ->
    ID.

remove({?MODULE, [ID]}) ->
    erlubi:callv('ubigraph.remove_edge', [ID]).


set(Att, Value, {?MODULE, [ID]}) ->
    erlubi:callv('ubigraph.set_edge_attribute',
                 [ID,
                  erlubi_util:to_string(Att),
                  erlubi_util:to_string(Value)]).


arrow(Boolean, {?MODULE, [_ID]}=THIS) when is_boolean(Boolean) ->
    set("arrow", Boolean, THIS).

arrow_position(Where, {?MODULE, [_ID]}=THIS) when 0.0 =< Where, Where =< 1.0 ->
    set("arrow_position", lists:flatten( io_lib:format("~f", [Where])), THIS).

spline(Boolean, {?MODULE, [_ID]}=THIS) when is_boolean(Boolean) ->
    set("spline", Boolean, THIS).

stroke(solid, {?MODULE, [_ID]}=THIS) ->
    set("stroke", "solid", THIS);
stroke(dashed, {?MODULE, [_ID]}=THIS) ->
    set("stroke", "dashed", THIS);
stroke(none, {?MODULE, [_ID]}=THIS) ->
    set("stroke", "none", THIS).

visible(Boolean, {?MODULE, [_ID]}=THIS) when is_boolean(Boolean) ->
    set("visible", Boolean, THIS).




