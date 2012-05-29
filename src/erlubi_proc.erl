-module(erlubi_proc).

-compile(export_all).

show() ->
    All = erlang:processes() ++ erlang:ports(),
    Dict = lists:foldl(fun(PID, Dict) ->
                               {ok, V} = erlubi:vertex(),

                               case catch erlang:process_info(PID, registered_name) of
                                   {registered_name, Name} ->
                                       ok = V:label(atom_to_list(Name));
                                   _ ->
                                       case catch erlang:port_info(PID, name) of
                                           {name, Name} ->
                                               V:color({0,255,0}),
                                               ok = V:label(Name);
                                           _ ->
                                               V:color({255,0,0}),
                                               ok % = V:label(PID)
                                       end
                               end,

                               dict:store(PID, V, Dict)
                        end,
                        dict:new(),
                        All),

    lists:foreach(fun(PID) ->
                          Vpid = dict:fetch(PID, Dict),

                          case catch erlang:process_info(PID, links) of
                              {links, LinkList} ->
                                  lists:foreach(fun(PID2) when PID<PID2 ->
                                                        {ok, E} = erlubi:edge(Vpid, dict:fetch(PID2, Dict)),
                                                        ok;
                                                   (_) ->
                                                        ok
                                                end,
                                                LinkList);
                              _ ->
                                  ok
                          end,

                          case catch erlang:port_info(PID, links) of
                              {links, LinkList2} ->
                                  lists:foreach(fun(PID2) when PID<PID2 ->
                                                        erlubi:edge(Vpid, dict:fetch(PID2, Dict));
                                                   (_) ->
                                                        ok
                                                end,
                                                LinkList2);
                              _ ->
                                  ok
                          end,


                          case catch erlang:process_info(PID, monitored_by) of
                              {monitored_by, MonList} ->
                                  lists:foreach(fun(PID2) ->
                                                        {ok, E} = erlubi:edge(dict:fetch(PID2, Dict), Vpid),
                                                        E:arrow(true),
                                                        E:stroke(dashed)
                                                end,
                                                MonList);
                              _ ->
                                  ok
                          end
                  end,
                  All).






