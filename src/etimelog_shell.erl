% Copyright 2011, Felix Lange <fjl@twurst.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

-module(etimelog_shell).
-export([start/0, start/1, start_shell/1]).
-export([complete_input/1]).

-include("etimelog.hrl").
-define(USER_DRV, 'tty_sl -c -e').

start() ->
    application:start(etimelog),
    start(init:get_plain_arguments()).

start([]) ->
    %% no args -- run interactive
    patch_user_drv(),
    user_drv:start(?USER_DRV, {?MODULE, start_shell, [undefined]});
start(Cmd) ->
    %% run the given command
    user:start(),
    case run_command(Cmd) of
        {error, Message} ->
            io:format("error: ~s~n", [Message]),
            halt(2);
        {ok, Output} ->
            io:format("~s~n", Output),
            halt(0);
        ok ->
            halt(0)
    end.

banner() ->
    {ok, Vsn} = application:get_key(etimelog, vsn),
    io:format("=== etimelog (~s)~n", [Vsn]).

start_shell(undefined) ->
    spawn_link(fun () ->
                       banner(),
                       io:setopts([{encoding, utf8}, {expand_fun, fun complete_input/1}]),
                       input_loop()
               end);
start_shell(PrevShell) when is_pid(PrevShell) ->
    PrevShell ! {new_group, group_leader()},
    PrevShell.

stop_user_drv() ->
    UserDrv = whereis(user_drv),
    exit(UserDrv, please_die),

    %% close user_drv port
    PortName = atom_to_list(?USER_DRV),
    [Port] = [P || P <- erlang:ports(), proplists:get_value(name, erlang:port_info(P)) =:= PortName],
    erlang:port_close(Port).

restart_user_drv() ->
    user_drv:start(?USER_DRV, {?MODULE, start_shell, [self()]}),
    receive
        {new_group, Group} ->
            io:setopts(Group, [{encoding, utf8}, {expand_fun, fun complete_input/1}]),
            group_leader(Group, self())
    end.

%% --------------------------------------------------------------------------------
%% -- interpreter
input_loop() ->
    case io:get_line("> ") of
        eof ->
            io:format("~nbye~n", []),
            halt(0);
        Data when is_list(Data) ->
            case do_input_line(string:strip(string:strip(Data, right, $\n), both, $\s)) of
                {error, Message} ->
                    io:format("error: ~s~n", [Message]),
                    input_loop();
                {ok, Output} ->
                    io:format("~s~n", Output),
                    input_loop();
                ok ->
                    input_loop()
            end
    end.

do_input_line("," ++ Line) ->
    run_command(string:tokens(Line, " "));
do_input_line("") ->
    {error, "nothing entered"};
do_input_line(Input) ->
    etimelog_file:add_entry(Input).

run_command(["all"]) ->
    Tab = [[day_to_table(Day, DayEntries), {""}] || {Day, DayEntries} <- lists:reverse(etimelog_file:all_entries())],
    show_table(lists:flatten(Tab));
run_command(["last"]) ->
    {Today, Now} = calendar:local_time(),
    case etimelog_file:last_entry() of
        undefined ->
            io:format("no entries~n");
        {Today, Entry} ->
            TimeSince = format_duration(etimelog_file:time_diff(Entry#entry.time, {Today, Now})),
            io:format("~s ago: ~s~n", [TimeSince, Entry#entry.text]);
        {EntryDay, Entry} ->
            io:format("on ~s: ~s~n", [format_day(EntryDay), Entry#entry.text])
    end;
run_command(["today"]) ->
    {Today, Entries} = etimelog_file:today_entries(),
    show_table(day_to_table(Today, Entries)),
    {WorkTime, SlackTime} = sum_times(Entries),
    (WorkTime > 0) andalso io:format("total: ~s (** ~s)~n", [format_duration(WorkTime), format_duration(SlackTime)]),
    ok;
run_command(["edit"]) ->
    case get_editor() of
        {error, unset} ->
            {error, "neither VISUAL nor EDITOR are set"};
        {ok, EditorCmd} ->
            launch_editor(EditorCmd, etimelog_file:filename()),
            etimelog_file:refresh()
    end;
run_command(["refresh"]) ->
    etimelog_file:refresh();
run_command(["quit"]) ->
    io:format("bye"),
    halt(0);
run_command(Other) ->
    {error, ["unknown command: ,", string:join(Other, " ")]}.

sum_times(Entries) ->
    lists:foldl(fun (#entry{duration = D, tag = Activity}, {WT, ST}) ->
                        case Activity of
                            regular  -> {WT + D, ST};
                            slacking -> {WT, ST + D};
                            excluded -> {WT, ST}
                        end
                end, {0, 0}, Entries).

%% --------------------------------------------------------------------------------
%% -- output
day_to_table(Day, Entries) ->
    [{format_day(Day)} | entry_table(lists:reverse(Entries))].

entry_table([]) ->
    [{"no entries"}];
entry_table(Entries) ->
    lists:map(fun entry_row/1, Entries).

entry_row(#entry{first_of_day = true, time = {_Day, Time}, text = Text}) ->
    {"[", format_time(Time), "", "", "] ", Text};
entry_row(#entry{duration = Secs, time = {_Day, Time}, text = Text}) ->
    Duration = format_duration(Secs),
    {"[", format_time(Time), " - ", Duration, "] ", Text}.

format_duration(0) ->
    "0m";
format_duration(Seconds) ->
    format_duration(Seconds, [{3600, "h"}, {60, "m"}, {1, "s"}]).

format_duration(0, _) ->
    [];
format_duration(Seconds, [{N, Unit} | DispSpec]) ->
    if
        Seconds >= N ->
            Value = Seconds div N,
            Rest  = Seconds rem N,
            [integer_to_list(Value), Unit | format_duration(Rest, DispSpec)];
        true ->
            format_duration(Seconds, DispSpec)
    end.

format_day({Year, Month, Day}) ->
    WeekDay  = case calendar:day_of_the_week(Year, Month, Day) of
                   1 -> "Monday";
                   2 -> "Tuesday";
                   3 -> "Wednesday";
                   4 -> "Thursday";
                   5 -> "Friday";
                   6 -> "Saturday";
                   7 -> "Sunday"
               end,
    MonthStr = case Month of
                   1  -> "January";
                   2  -> "February";
                   3  -> "March";
                   4  -> "April";
                   5  -> "May";
                   6  -> "June";
                   7  -> "July";
                   8  -> "August";
                   9  -> "September";
                   10 -> "October";
                   11 -> "November";
                   12 -> "December"
               end,
    [WeekDay, ", ", ordnum(integer_to_list(Day)), " ", MonthStr, " ", integer_to_list(Year)].

format_time({Hours, Minutes, _Seconds}) ->
    io_lib:format("~2..0b:~2..0b", [Hours, Minutes]).

ordnum("1")  -> "1st";
ordnum("2")  -> "2nd";
ordnum("11") -> "11th";
ordnum("12") -> "12th";
ordnum(N) ->
    case lists:last(N) of
        1 -> [N, "st"];
        2 -> [N, "nd"];
        _ -> [N, "th"]
    end.

show_table(Table) ->
    show_table(Table, "").
show_table(Table, ColSeparator) ->
    Widths = column_widths(Table),
    lists:foreach(fun (Row) ->
                          LastCol = tuple_size(Row),
                          lists:foldl(fun (Col, N) when N == LastCol ->
                                              io:put_chars([Col, $\n]); %% don't pad the last column
                                          (Col, N) ->
                                              io:put_chars([pad_str(Col, proplists:get_value(N, Widths)), ColSeparator]),
                                              N + 1
                                      end, 1, tuple_to_list(Row))
                  end, Table).

column_widths(Table) ->
    lists:foldl(fun ({_OneElementRow}, OuterAcc) ->
                        OuterAcc; %% ignore one-element rows in width calculation
                    (Row, OuterAcc) ->
                        {_, NewAcc} = lists:foldl(fun (Col, {N, InnerAcc}) ->
                                                          LastWidth = proplists:get_value(N, InnerAcc, 0),
                                                          ColWidth = iolist_size(Col),
                                                          {N + 1, lists:keystore(N, 1, InnerAcc, {N, max(LastWidth, ColWidth)})}
                                                  end, {1, OuterAcc}, tuple_to_list(Row)),
                        NewAcc
                end, [], Table).

pad_str(Str, N) ->
    case iolist_size(Str) of
        Len when Len < N -> [Str, lists:duplicate(N - Len, $\s)];
        _                -> Str
    end.

%% --------------------------------------------------------------------------------
%% -- editing
get_editor() ->
    case os:getenv("VISUAL") of
        Str when Str /= false, Str /= "" ->
            {ok, string:tokens(Str, " ")};
        _ ->
            case os:getenv("EDITOR") of
                Str when Str /= false, Str /= "" ->
                    {ok, string:tokens(Str, " ")};
                _ ->
                    {error, unset}
            end
    end.

launch_editor([EditorCmd | EditorArgs], TimelogFile) ->
    stop_user_drv(),
    try erlang:open_port({spawn_executable, os:find_executable(EditorCmd)}, [{args, EditorArgs ++ [TimelogFile]}, exit_status, nouse_stdio]) of
        Port ->
            io:format("waiting for editor \"~s\"~n", [string:join([EditorCmd | EditorArgs], " ")]),
            wait_editor_exit(Port)
    catch
        error:Error ->
            {error, io_lib:format("could not start editor: ~s~n", [file:format_error(Error)])}
    after
        restart_user_drv()
    end.

wait_editor_exit(Port) ->
    receive
        {Port, {exit_status, 0}} -> ok;
        {Port, {exit_status, Status}} ->
            {error, io_lib:format("editor exit status non-zero: ~p", [Status])}
    end.

%% --------------------------------------------------------------------------------
%% -- completion
all_commands() ->
    [{"all",     "show all timelog entries"},
     {"last",    "show the last entry"},
     {"edit",    "edit the timelog file"},
     {"refresh", "reload the timelog file"},
     {"today",   "show today's entries and work time"},
     {"quit",    "quit etimelog"}].

complete_input(Line) ->
    case re:run(Line, "^([a-z]*),$", [{capture, all_but_first, list}]) of
        {match, [RevCommand]} ->
            command_completions(lists:reverse(RevCommand));
        nomatch ->
            {yes, " ", []}
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

%% --------------------------------------------------------------------------------
%% -- user_drv hot patching (strip out call to system_info(version))
patch_user_drv() ->
    patch_module(user_drv, fun transform_user_drv/1).

transform_user_drv({call, Line,
                       {remote,_,{atom,_,io_lib},{atom,_,format}},
                       [{string,_,"~s\n"},
                        {cons,_,
                            {call,_,
                                {remote,_,{atom,_,erlang},{atom,_,system_info}},
                                [{atom,_,system_version}]},
                            {nil,132}}]}) ->
    {replace, {string, Line, ""}};

%% not strictly necessary, just for forward compatibility
transform_user_drv({call, Line, {remote, _, {atom,_,erlang}, {atom,_,system_info}}, [{atom,_,system_version}]}) ->
    {replace, {string, Line, ""}};

transform_user_drv(AnythingElse) ->
    {descend, AnythingElse}.

patch_module(Mod, Transform) ->
    File = code:which(Mod),
    {ok, {Mod, [{abstract_code, {raw_abstract_v1, Code}}]}} = beam_lib:chunks(File, [abstract_code]),
    NewCode = patch_abstract_code(Code, Transform),
    {ok, Mod, NewCodeBin} = compile:forms(NewCode, [binary]),
    code:unstick_mod(Mod),
    code:load_binary(Mod, File, NewCodeBin).

patch_abstract_code(Code, Transform) when is_tuple(Code) ->
    case Transform(Code) of
        {replace, NewCode} -> NewCode;
        {descend, NewCode} -> list_to_tuple([patch_abstract_code(S, Transform) || S <- tuple_to_list(NewCode)])
    end;
patch_abstract_code(Code, Transform) when is_list(Code) ->
    [patch_abstract_code(S, Transform) || S <- Code];
patch_abstract_code(Code, _Transform) ->
    Code.
