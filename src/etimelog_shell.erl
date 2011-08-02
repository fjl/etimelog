-module(etimelog_shell).
-export([start/0, start_shell/0]).
-export([complete_input/1]).

start() ->
    user_drv:start('tty_sl -c -e', {?MODULE, start_shell, []}),
    timer:sleep(infinity).

banner() ->
    {ok, Vsn} = application:get_key(etimelog, vsn),
    io:format("=== etimelog (~s)~n", [Vsn]).

start_shell() ->
    spawn(fun () ->
                application:start(etimelog),
                banner(),
                io:setopts([{encoding, utf8}, {expand_fun, fun complete_input/1}]),
                input_loop()
          end).

input_loop() ->
    case io:get_line("> ") of
        eof ->
            io:format("~nbye~n", []),
            halt(0);
        Data when is_list(Data) ->
            case do_input_line(Data) of
                {error, Message} ->
                    io:format("error: ~s~n", [Message]);
                {ok, Output} ->
                    io:format("~s~n", Output);
                ok ->
                    io:format("ok~n", []);
                quit ->
                    io:format("bye~n", []),
                    halt(0)
            end,
            input_loop()
    end.

do_input_line("," ++ Line) ->
    Input = string:tokens(string:strip(Line, right, $\n), " "),
    run_command(Input);
do_input_line(Line) ->
    Input = string:strip(Line, right, $\n),
    etimelog_file:add_entry(Input).

run_command(["all"]) ->
    {ok, Entries} = etimelog_file:all_entries(),
    {ok, io_lib:format("~p", [Entries])};
run_command(["quit"]) ->
    quit;
run_command(Other) ->
    {error, ["unknown command: ,", string:join(Other, " ")]}.

all_commands() ->
    [{"all",  "show all timelog entries"},
     {"quit", "quit etimelog"}].

complete_input(Line) ->
    case re:run(Line, "^([a-z]*),$", [{capture, all_but_first, list}]) of
        {match, [RevCommand]} ->
            command_completions(lists:reverse(RevCommand));
        nomatch ->
            {yes, "\t", []}
    end.

command_completions(Str) ->
    case lists:filter(fun ({Cmd, _}) -> lists:prefix(Str, Cmd) end, all_commands()) of
        [] ->
            {no, [], []};
        [{Cmd, _Desc}] ->
            if
                Cmd == Str -> {yes, [], []};
                true       -> {yes, string:substr(Cmd, length(Str) + 1), []}
            end;
        MatchedCommands ->
            {no, [], lists:map(fun ({Cmd, Desc}) -> Cmd ++ " -- " ++ Desc ++ "           " end, MatchedCommands)}
    end.
