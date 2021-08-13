-module(ecg).
-mode(compile).

%% API exports
-export([main/1,
         run/2]).

main(Args) ->
    [Name | Modules] = Args,
    print("~ts\n", [run(Name, Modules)]).

run(Name, ModulePaths) ->
    check_modules(ModulePaths),
    {ok, Calls} = get_calls(ModulePaths),
    Modules = [list_to_atom(filename:rootname(filename:basename(MP))) || MP <- ModulePaths],
    %print("calls: ~p\n", [Calls]),
    make_dot(Name, Modules, Calls).

check_modules(Modules) ->
    case catch lists:all(fun
                             (true) -> true;
                             (Err)  -> throw(Err)
                         end, [ check_module(M) || M <- Modules ])
    of
        true -> ok;
        {_, _} = Reason -> error(Reason, [Modules])
    end.

check_module(MPath) ->
    case filelib:is_regular(MPath) of
        true -> true;
        false -> {cannot_access, MPath}
    end.

get_calls(MPaths) ->
    {ok, _} = xref:start(xr()),
    [ {ok, _} = xref:add_module(xr(), M) || M <- MPaths ],
    {ok, _} = xref:q(xr(), "E", [{verbose, false}]).

make_dot(Name, TargetModules, Edges) ->
    Vertices = all_vertices(Edges),
    ["digraph ", Name, " {\n",
     style_header(),
     make_dot_vertices(TargetModules, Vertices),
     make_dot_(Edges),
     "}\n"].

style_header() ->
    ["  ranksep=\"2.0 equally\";\n"].

make_dot_(Edges) ->
    [ ["  ", format_mfa(From), " -> ", format_mfa(To), ";\n"]
      || {From, To} <- Edges ].

format_mfa({M, F, A}) -> io_lib:format("\"~ts:~ts/~b\"", [M, F, A]).
format_fa({_M, F, A}) -> io_lib:format("\"~ts/~b\"", [F, A]).

print(Fmt, Args) ->
    io:format(Fmt, Args).

%% xref handle
xr() -> ?MODULE.

all_vertices(Edges) ->
    lists:usort(lists:flatten(lists:map(fun erlang:tuple_to_list/1, Edges))).

make_dot_vertices(TargetModules, Vertices) ->
    GroupedVertices = group_vertices(Vertices),
    SelectedGroupedVertices = maps:with(TargetModules, GroupedVertices),
    [make_module_vertices(M, MVertices) || {M, MVertices} <- maps:to_list(SelectedGroupedVertices)].

make_module_vertices(Module, Vertices) ->
    ModName = io_lib:format("~p", [Module]),
    ["  subgraph cluster_", ModName, " {\n",
      "    label=", ModName, "\n",
      "    color=blue;\n",
     [["    ", format_mfa(Vertex), " [label=", format_fa(Vertex), "];\n"] || Vertex <- Vertices],
     "  }\n\n"
    ].

group_vertices(Vertices) ->
    lists:foldl(
        fun({M, _F, _A} = MFA, Acc) ->
            Acc#{M => [MFA | maps:get(M, Acc, [])]}
        end,
        #{},
        Vertices
    ).
