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

 -module(erlubi_tracer).


-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, stop/0, run/0]).


-define(PORT_COLOR, {0,255,0}).
-define(PID_COLOR, {255,0,0}).
-define(NAMED_COLOR, {0,0,255}).

run() ->
    erlubi:start(),
    erlubi:clear(),
    erlubi_tracer:start().


-record(state, { pdict=dict:new(), ldict=dict:new() }).
-record(process, { vertex, monitor_arg, monitors=[] }).


start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).


%% Server implementation, a.k.a.: callbacks

init([]) ->
    State0 = #state{},

    State1 = lists:foldl(fun init_pid/2, State0, erlang:processes()),
    State2 = lists:foldl(fun init_pid/2, State1, erlang:ports()),

    erlang:trace(all, true, [procs,call]),
    erlang:trace(self(), false, [all]),
    erlang:trace(whereis(erlubi), false, [all]),

    erlang:trace_pattern({erlang, monitor, 2}, [{['_','_'],[],[{return_trace}]}], []),
    erlang:trace_pattern({erlang, demonitor, 1}, true, []),

    {ok, State2}.


init_pid(PID,State) ->
    {Vertex,State2} = vertex(PID, State),

    State3 = init_pid_label(PID,Vertex,State2),
    State4 = init_pid_links(PID,State3),
    State5 = init_pid_monitors(PID,State4),

    State5.

init_pid_label(PID,Vertex,State) when is_pid(PID) ->
    case catch erlang:process_info(PID, registered_name) of
        {registered_name, Name} ->
            Vertex:color(?NAMED_COLOR),
            ok = Vertex:label(atom_to_list(Name));
        _ ->
            ok = Vertex:label(PID)
    end,
    State;

init_pid_label(Port,Vertex, State) when is_port(Port) ->
    case catch erlang:port_info(Port, name) of
        {name, Name} ->
            ok = Vertex:label(Name);
        _ ->
            ok = Vertex:label(Port)
    end,
    State.

init_pid_links(PID,State) ->
    case catch erlang:process_info(PID, links) of
        {links, LinkList} ->
            lists:foldl(fun(PID2,State1) ->
                                add_link(PID,PID2,State1)
                        end,
                        State,
                        LinkList);
        _ ->
            State
    end.

init_pid_monitors(_PID,State) ->
    State.


handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
  io:format("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.


handle_cast(_Msg, State) ->
  io:format("cast ~p, ~p.", [_Msg, State]),
  {noreply, State}.


handle_info({trace, PID, spawn, NewPID, _MFA}, State) when PID =/= self() ->
    {_, State2} = vertex(NewPID, State),
    {noreply, State2};

handle_info({trace, _, spawn, _, _}, State) ->
    {noreply, State};

handle_info({trace, PID, exit, _Reason}, State= #state{ pdict=PDict }) ->
    case dict:find(PID, PDict) of
        {ok, #process{ vertex=V }} ->
            V:remove(),
            {noreply, State#state{ pdict=dict:erase(PID, PDict) }};
        _Result ->
            io:format("process unknown ~p~n", [_Result]),
            {noreply, State}
    end;

handle_info({trace, PID, register, Name}, State) ->
    {V, State2} = vertex(PID, State),
    V:color(?NAMED_COLOR),
    V:label(Name),
    {noreply, State2};

handle_info({trace, PID, unregister, _Name}, State) ->
    {V, State2} = vertex(PID, State),
    V:color(?PID_COLOR),
    V:label(PID),
    {noreply, State2};

handle_info({trace, PID1, link, PID2}, State) ->
    {noreply, add_link(PID1, PID2, State)};

handle_info({trace, _PID1, getting_linked, _PID2}, State) ->
    {noreply, State};


handle_info({trace, PID1, unlink, PID2}, State) ->
    {noreply, remove_link(PID1, PID2, State)};

handle_info({trace, _PID1, getting_unlinked, _PID2}, State) ->
    {noreply, State};

handle_info({trace, PID, call, {erlang, monitor, [Type, Item]}}, State) ->
    Process = dict:fetch(PID, State#state.pdict),
    {noreply, State#state{ pdict=dict:store(PID, Process#process{ monitor_arg={Type,Item} }, State#state.pdict) }};

handle_info({trace, PID, return_from, {erlang, monitor, 2}, MRef}, State) ->
    #process{ monitor_arg=Args } = dict:fetch(PID, State#state.pdict),
    case Args of
        {process, {RegName, Node}} ->
            case node() of
                Node ->
                    PID2 = whereis(RegName),
                    {noreply, add_monitor(PID, PID2, MRef, State)};
                _OtherNode ->
                    {noreply, State}
            end;
        {process, PID2} when is_pid(PID2) ->
            {noreply, add_monitor(PID, PID2, MRef, State)};

        {process, RegName} ->
            PID2 = whereis(RegName),
            if is_pid(PID2) ->
                    {noreply, add_monitor(PID, PID2, MRef, State)};
               true ->
                    {noreply, State}
            end
    end;

handle_info({trace, PID, call, {erlang, demonitor, [MRef]}}, State) ->
    {noreply, remove_monitor(PID, MRef, State)};

handle_info(_Info, State) ->
  io:format("info ~p, ~p.", [_Info, State]),
  {noreply, State}.


terminate(_Reason, _State) ->
    erlang:trace(all, false, [procs, call]),
    ok.


code_change(_OldVsn, State, _Extra) ->
  io:format("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
  {ok, State}.




vertex(PID, State=#state{ pdict=PDict }) ->
    case dict:find(PID, PDict) of
        {ok, #process{ vertex=V }} ->
            {V, State};
        error ->
            {ok, V} = erlubi:vertex(),
            if is_pid(PID) ->
                    V:shape(sphere),
                    V:label(PID),
                    V:color(?PID_COLOR);
               is_port(PID) ->
                    V:color(?PORT_COLOR)
            end,
            {V, State#state{ pdict=dict:store(PID, #process{ vertex=V }, PDict) }}
    end.



add_link(PID1, PID2, State) when PID1>PID2 ->
    add_link(PID2, PID1, State);

add_link(PID1, PID2, State) ->
    case dict:find({PID1,PID2}, State#state.ldict) of
        {ok, _E} ->
            State;
        error ->
            {V1, State1} = vertex(PID1, State),
            {V2, State2} = vertex(PID2, State1),
            {ok, E} = erlubi:edge(V1, V2),
            State2#state{ ldict = dict:store({PID1,PID2}, E, State2#state.ldict) }
    end.

remove_link(PID1, PID2, State) when PID1>PID2 ->
    remove_link(PID2, PID1, State);
remove_link(PID1, PID2, State) ->
    case dict:find({PID1,PID2}, State#state.ldict) of
        {ok, E} ->
            E:remove(),
            State#state{ ldict=dict:erase({PID1,PID2}, State#state.ldict) };
        error ->
            State
    end.


add_monitor(PID1, PID2, MRef, State0) ->
    {V1, State1} = vertex(PID1, State0),
    {V2, State2} = vertex(PID2, State1),
    {ok, E} = erlubi:edge(V1, V2),
    State2#state{ pdict=dict:update( PID1,
                                    fun(Process=#process{ monitors=Monitors }) ->
                                            Process#process{
                                              monitors=orddict:store(MRef, {PID2, E}, Monitors),
                                              monitor_arg=undefined
                                             }
                                    end,
                                    undefined,
                                    State2#state.pdict ) }.

remove_monitor(PID1, MRef, State) ->
    State#state{ pdict=dict:update( PID1,
                                    fun(Process=#process{ monitors=Monitors }) ->
                                            {_PID2, E} = orddict:fetch(MRef, Monitors),
                                            E:remove(),
                                            Process#process{ monitors=orddict:erase(MRef, Monitors) }
                                    end,
                                    undefined,
                                    State#state.pdict ) }.


