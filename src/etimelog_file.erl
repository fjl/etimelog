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

-module(etimelog_file).
-behaviour(gen_server).

-export([start_link/1, add_entry/1, all_entries/0, today_entries/0, day_entries/1,
         last_entry/0, refresh/0, filename/0]).
-export([time_diff/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, etimelog_file).
-include("etimelog.hrl").

%% --------------------------------------------------------------------------------
%% -- API
start_link(LogFilename) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {LogFilename}, []).

add_entry(Text) ->
    gen_server:call(?SERVER, {entry, make_entry(Text)}).

all_entries() ->
    gen_server:call(?SERVER, all_entries).

today_entries() ->
    day_entries(calendar:local_time()).

day_entries(Day = {_, _, _}) ->
    day_entries({Day, {0,0,0}});
day_entries(DateTime = {{_, _, _}, {_, _, _}}) ->
    gen_server:call(?SERVER, {day_entries, DateTime}).

last_entry() ->
    gen_server:call(?SERVER, last_entry).

refresh() ->
    gen_server:call(?SERVER, refresh).

filename() ->
    gen_server:call(?SERVER, filename).

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {filename, entries}).

init({LogFilename}) ->
    touch(LogFilename),
    {ok, Entries} = logfile_entries(LogFilename),
    {ok, #state{filename = LogFilename, entries = Entries}}.

handle_call({entry, NewEntry}, _From, State = #state{filename = LogFilename, entries = Entries}) ->
    case write_entry(LogFilename, NewEntry) of
        {error, Error} ->
            {reply, {error, Error}, State};
        ok ->
            NewState = State#state{entries = collect_entry(NewEntry, get_virtual_midnight(), Entries)},
            {reply, ok, NewState}
    end;

handle_call(all_entries, _From, State = #state{entries = Entries}) ->
    {reply, Entries, State};

handle_call({day_entries, DateTime}, _From, State = #state{entries = Entries}) ->
    PrevDay = prev_day(DateTime),
    case on_same_day(DateTime, PrevDay, get_virtual_midnight()) of
        true  -> {LookupDay, _} = PrevDay;
        false -> {LookupDay, _} = DateTime
    end,
    {reply, {LookupDay, proplists:get_value(LookupDay, Entries, [])}, State};

handle_call(last_entry, _From, State = #state{entries = Entries}) ->
    case Entries of
        [] ->
            {reply, undefined, State};
        [{Day, [FirstEntry | _]} | _] ->
            {reply, {Day, FirstEntry}, State}
    end;

handle_call(refresh, _From, State = #state{filename = LogFilename}) ->
    {ok, NewEntries} = logfile_entries(LogFilename),
    NewState = State#state{entries = NewEntries},
    {reply, ok, NewState};

handle_call(filename, _From, State = #state{filename = Filename}) ->
    {reply, Filename, State};

handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% unused
terminate(_Reason, _State) ->
    ok.
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.
handle_cast(_Cast, State) ->
    {noreply, State}.
handle_info(_InfoMsg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------
%% -- helpers
collect_entry(Entry = #entry{time = {Day, _Time}}, _VirtualMidnight, []) ->
    [{Day, [Entry#entry{duration = 0, first_of_day = true}]}];
collect_entry(Entry, VirtualMidnight, Acc = [{LastDay, LastDayAcc = [LastEntry | _]} | AccRest]) ->
    case on_same_day(Entry#entry.time, {LastDay, {0,0,0}}, VirtualMidnight) of
        true ->
            NewEntry = Entry#entry{first_of_day = false, duration = time_diff(LastEntry#entry.time, Entry#entry.time)},
            [{LastDay, [NewEntry | LastDayAcc]} | AccRest];
        false ->
            NewDay = element(1, Entry#entry.time),
            NewEntry = Entry#entry{first_of_day = true, duration = 0},
            [{NewDay, [NewEntry]} | Acc]
    end.

on_same_day({CurrentDay, CurrentTime}, {LastDay, LastTime}, VirtualMidnight) ->
    {NextDay, _} = next_day({LastDay, LastTime}),
    case CurrentDay of
        LastDay                                     -> true;
        NextDay when CurrentTime =< VirtualMidnight -> true;
        _                                           -> false
    end.

next_day(Datetime) ->
    calendar:gregorian_seconds_to_datetime(86400 + calendar:datetime_to_gregorian_seconds(Datetime)).

prev_day(Datetime) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Datetime) - 86400).

time_diff(Date1, Date2) ->
    calendar:datetime_to_gregorian_seconds(Date2) - calendar:datetime_to_gregorian_seconds(Date1).

get_virtual_midnight() ->
    {ok, MidnightSpec} = application:get_env(etimelog, virtual_midnight),
    parse_timespec(MidnightSpec).

%% --------------------------------------------------------------------------------
%% -- file parsing and writing
touch(Filename) ->
    filelib:ensure_dir(Filename),
    {ok, File} = file:open(Filename, [raw, append]),
    file:close(File).

with_logfile(Fun, Filename, ModeList) ->
    {ok, File} = file:open(Filename, [raw, binary | ModeList]),
    try
        Fun(File)
    after
        file:close(File)
    end.

logfile_entries(Filename) ->
    with_logfile(fun (File) ->
                         VirtualMidnight = get_virtual_midnight(),
                         fold_entries(fun (E, Acc) -> collect_entry(E, VirtualMidnight, Acc) end, [], File)
                 end, Filename, [read, read_ahead]).

write_entry(Filename, #entry{time = Time, text = Text}) ->
    with_logfile(fun (File) ->
                         EntryLine = [fmt_time(local_to_utc(Time)), ": ", Text, "\n"],
                         file:write(File, EntryLine)
                 end, Filename, [write, append]).

fmt_time({{Year, Month, Day}, {Hour, Min, _Sec}}) ->
    io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b", [Year, Month, Day, Hour, Min]).

make_entry(Text) ->
    {Day, {Hours, Minutes, _Seconds}} = calendar:local_time(),
    (parse_text(Text))#entry{time = {Day, {Hours, Minutes, 0}}}.

parse_text(Text) ->
    Stripped = string:strip(string:strip(Text, both, $\s), right, $\n),
    Tag      = get_tag(Stripped),
    #entry{tag = Tag, text = Stripped}.

%% parses gtimelog-style entries:
%%   three starts at the end: excluded entry
%%   two stars at the end: slacking entry
%%   anything else: regular entry
get_tag(Text) ->
    case re:run(Text, " [*]{3}$", [{capture, none}]) of
        match   -> excluded;
        nomatch ->
            case re:run(Text, " [*]{2}", [{capture, none}]) of
                match   -> slacking;
                nomatch -> regular
            end
    end.

%% Str is a timespec in local time, e.g. "08:42", "9:30" or "14:55"
parse_timespec(Str) ->
    case re:run(Str, "^([0-9]{1,2}):([0-9]{2})$", [{capture, all_but_first, list}]) of
        {match, [HS, MS]} ->
            {list_to_integer(HS), list_to_integer(MS), 0};
        nomatch ->
            undefined
    end.

fold_entries(Fun, Acc, File) ->
    case file:read_line(File) of
        {ok, Line} ->
            case parse_line(Line) of
                E = #entry{} ->
                    fold_entries(Fun, Fun(E, Acc), File);
                undefined ->
                    fold_entries(Fun, Acc, File)
            end;
        eof ->
            {ok, Acc};
        {error, Error} ->
            {error, Error}
    end.

-define(TS_Regex, "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}").

parse_line(Line) ->
    case re:run(Line, "^(" ?TS_Regex "): (.*)\n$", [{capture, all_but_first, list}]) of
        {match, [Timestamp, Text]} ->
            {ok, TS} = read_date(Timestamp),
            (parse_text(Text))#entry{time = utc_to_local(TS)};
        nomatch ->
            undefined
    end.

read_date(Str) ->
    case io_lib:fread("~4d-~2d-~2d ~2d:~2d", Str) of
        {ok, [Year, Month, Day, Hour, Minute], _} ->
            {ok, {{Year, Month, Day}, {Hour, Minute, 0}}};
        {error, Error} ->
            {error, Error}
    end.

utc_to_local(DateTime) ->
    calendar:universal_time_to_local_time(DateTime).
local_to_utc(DateTime) ->
    case calendar:local_time_to_universal_time_dst(DateTime) of
        [LocalTime]    -> LocalTime;
        [LocalTime, _] -> LocalTime  %% honestly, what's the correct answer here?
    end.
