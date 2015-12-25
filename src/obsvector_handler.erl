-module(obsvector_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
        {ok, Req, undefined}.
%%TODOS:
%% -make it a hidden node?

handle(Req, State) ->
    {P, Req2} = cowboy_req:qs_val(<<"pid">>, Req, undefined),
    Json =
        case P of
            undefined -> process_list_json();
            _ -> case parse_pid(P) of
                    {ok, Pid} -> process_status_json(Pid);
                    _ -> <<"not a pid">>
                end
        end,
    {ok, Req3} =
        cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Json, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
        ok.

parse_pid(P) ->
    try {ok, list_to_pid(binary_to_list(P))}
    catch error:badarg -> {error, not_parseable, [{pid, P}]}
    end.

process_list_json() ->
    EJson =
        lists:map(
      fun(Pid) ->
              Info = [{pid, Pid}| erlang:process_info(Pid)],
              process_to_ejson(Info)
      end, erlang:processes()),
    jiffy:encode(EJson).

process_to_ejson(Info) ->
    R = any_to_ejson(Info),
    R.


any_to_ejson(I) when is_number(I) -> I;
any_to_ejson([]) -> [];
any_to_ejson(M) when is_map(M) -> any_to_ejson(maps:to_list(M));
any_to_ejson(L0) when is_list(L0) ->
    L = clean_improper_list(L0),
    case is_proplist(L) of
        true -> proplist_to_ejson(L);
        false -> case io_lib:printable_list(L) of
                     true -> unicode:characters_to_binary(L);
                     false -> lists:map(fun any_to_ejson/1, L)
                 end
    end;
any_to_ejson(A) when is_atom(A) -> A;
any_to_ejson(T) when is_tuple(T) ->
    [any_to_ejson(X) || X <- tuple_to_list(T)];
any_to_ejson(P) when is_pid(P) -> unicode:characters_to_binary(pid_to_list(P));
any_to_ejson(_) -> <<"not handled">>.

proplist_to_ejson(L) when is_list(L)->
    {[propentry_to_ejson(E) || E <-L]}.

propentry_to_ejson({K,V}) ->
    {K, any_to_ejson(V)};
propentry_to_ejson(A) when is_atom(A) ->
    {A, true}.

%% TODO: List of only atoms?
is_proplist(List) ->
    is_list(List) andalso
        lists:all(fun({K, _}) when is_atom(K) -> true;
                     (A) when is_atom(A) -> true;
                     (_)      -> false
                  end,
                  List).


clean_improper_list([H|T]) -> [H|clean_improper_list(T)];
clean_improper_list([]) -> [];
clean_improper_list(T) -> [T].

process_status_json(Pid) when is_pid(Pid) ->
    State = try sys:get_state(Pid, 1000)
            catch exit:{timeout,_} -> {error, timeout, []}
            end,
    EJson = any_to_eterm_ejson(State),
    jiffy:encode(EJson).

atej(T) -> any_to_eterm_ejson(T).
%% TODO proplist
any_to_eterm_ejson(T) when is_integer(T) -> eterm_tag("int", T);
any_to_eterm_ejson(T) when is_float(T) -> eterm_tag("float", T);
any_to_eterm_ejson(T) when is_boolean(T) -> eterm_tag("bool", T);
any_to_eterm_ejson(T) when is_atom(T) -> eterm_tag("atom", T);
any_to_eterm_ejson(T) when is_pid(T) -> eterm_tag("pid", to_str(T));
any_to_eterm_ejson(T) when is_binary(T) ->
    %% Workaround till I find a better solution
    case io_lib:printable_list(binary_to_list(T)) of
        true -> eterm_tag("binary_string", T);
        _ -> eterm_tag("binary", to_str("blob"))
    end;
any_to_eterm_ejson(T) when is_function(T) -> eterm_tag("fun", to_str(T));
%% TODO this fails for improper lists
any_to_eterm_ejson(T0) when is_list(T0) ->
    %% list might be improper
    T = clean_improper_list(T0),
    case io_lib:printable_list(T) of
                     true -> eterm_tag("string", to_str(T));
                     false -> eterm_tag("list",[atej(E)||E<-T])
    end;
any_to_eterm_ejson(T) when is_tuple(T) -> eterm_tag("tuple",[atej(E)||E<-tuple_to_list(T)]);
any_to_eterm_ejson(T) when is_map(T) ->
    KV = [[atej(K), atej(V)] || {K, V} <- maps:to_list(T)],
    eterm_tag("map", KV);
any_to_eterm_ejson(T) when is_reference(T) -> eterm_tag("ref", to_str(T));
any_to_eterm_ejson(_) -> eterm_tag("string", to_str("not handled")).


eterm_tag(Tag, Term) ->
    #{tag => to_str(Tag), term => Term}.

to_str(T) when is_list(T) ->
    unicode:characters_to_binary(T);
to_str(Any) ->
    unicode:characters_to_binary(io_lib:format("~p", [Any])).

