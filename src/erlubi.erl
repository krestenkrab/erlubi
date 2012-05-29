-module(erlubi).


-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, start/1, call/2, callv/2, stop/0]).

-export([clear/0, vertex/0, edge/2,
         remove_vertex/1, remove_edge/1]).

-record(state, {
          host="127.0.0.1" :: xmlrpc:host(),
          port=20738       :: pos_integer(),
          timeout=1000     :: pos_integer(),
          socket=undefined :: port() | undefined
         }).


%% Public API

start() ->
    start("127.0.0.1").

start(Host) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [Host,20738], []).

call(Method,Args) ->
    case gen_server:call(?MODULE, {call, Method, Args}) of
        {ok, {response, [N]}} ->
            {ok, N}
    end.

callv(Method,Args) ->
    case gen_server:call(?MODULE, {call, Method, Args}) of
        {ok, {response, [0]}} ->
            ok;
        {ok, {response, [-1]}} ->
            error
    end.

stop() ->
  gen_server:call(?MODULE, stop).

clear() ->
    {ok, 0} = call('ubigraph.clear', []),
    ok.

vertex() ->
    {ok, N} = call('ubigraph.new_vertex', []),
    {ok, {erlubi_vertex, N}}.

edge({erlubi_vertex, V1}, {erlubi_vertex, V2}) ->
    {ok, E} = call('ubigraph.new_edge', [V1, V2]),
    {ok, {erlubi_edge, E}}.

remove_vertex({erlubi_vertex, V}) ->
    callv('ubigraph.remove_vertex', [V]).

remove_edge({erlubi_edge, E}) ->
    callv('ubigraph.remove_edge', [E]).



%% Server implementation, a.k.a.: callbacks

init([Host,Port]) ->
  io:format("init", []),
  {ok, #state{ host=Host, port=Port }}.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call({call, Method, Args}, _From, State) ->
    {Response, State2} = do_call(State, {call, Method, Args}),
    {reply, Response, State2};

handle_call(_Request, _From, State) ->
  io:format("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.


handle_cast(_Msg, State) ->
  io:format("cast ~p, ~p.", [_Msg, State]),
  {noreply, State}.


handle_info(_Info, State) ->
  io:format("info ~p, ~p.", [_Info, State]),
  {noreply, State}.


terminate(_Reason, _State) ->
  io:format("terminate ~p, ~p", [_Reason, _State]),
  ok.


code_change(_OldVsn, State, _Extra) ->
  io:format("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
  {ok, State}.


%% internal

do_call(State = #state{ host=Host, port=Port, socket=undefined, timeout=Timeout },
        Payload) ->
    case xmlrpc:call(Host, Port, "/RPC2", Payload, false, Timeout) of
        {ok, Socket, Response} ->
            {{ok, Response}, State#state{ socket=Socket }};
        {ok, Response} ->
            {{ok, Response}, State};
        {error, Reason} ->
            {{error, Reason}, State};
        {error, _, Reason} ->
            {{error, Reason}, State#state{ socket=undefined }}
    end;

do_call(State = #state{ socket=Socket, timeout=Timeout }, Payload) ->
    case xmlrpc:call(Socket, "/RPC2", Payload, true, Timeout) of
        {ok, Socket, Response} ->
            {{ok, Response}, State};
        {error, _, closed} ->
            do_call(State#state{ socket=undefined }, Payload);
        {error, _, Reason} ->
            {{error, Reason}, State#state{ socket=undefined }}
    end.




