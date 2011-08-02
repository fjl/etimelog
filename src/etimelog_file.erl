-module(etimelog_file).
-behaviour(gen_server).

-export([start_link/1, add_entry/1, all_entries/0]).
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

%% --------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {filename, file}).

init({LogFilename}) ->
    case open_logfile(LogFilename) of
        {ok, LogFile} ->
            {ok, #state{filename = LogFilename, file = LogFile}};
        {error, Error} ->
            {stop, Error}
    end.

handle_call({entry, Entry = #entry{}}, _From, State = #state{file = LogFile}) ->
    case write_entry(LogFile, Entry) of
        ok             -> {reply, ok, State};
        {error, Error} -> {reply, {error, Error}, State}
    end;
handle_call(all_entries, _From, State = #state{file = LogFile}) ->
    Reply = fold_entries(fun (E, Acc) -> [E | Acc] end, [], LogFile),
    {reply, Reply, State};
handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

terminate(_Reason, State) ->
    file:close(State#state.file).

%% unused
code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.
handle_cast(_Cast, State) ->
    {noreply, State}.
handle_info(_InfoMsg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------------------
%% -- helpers
open_logfile(Filename) ->
    file:open(Filename, [write, read, read_ahead, raw]).

write_entry(File, #entry{time = Time, tag = Tag, text = Text}) ->
    EntryLine = [fmt_time(Time), " ", atom_to_list(Tag), " ", Text, "\n"],
    file:write(File, EntryLine),
    file:datasync(File).

fmt_time({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b", [Year, Month, Day, Hour, Min, Sec]).

make_entry(Text) ->
    Time = calendar:universal_time(),
    Tag  = get_tag(Text),
    #entry{time = Time, tag = Tag, text = Text}.

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

fold_entries(Fun, Acc0, File) ->
    file:position(File, bof),
    AccEnd = do_lines(File, Fun, Acc0),
    file:position(File, eof),
    AccEnd.

do_lines(File, Fun, Acc) ->
    case file:read_line(File) of
        {ok, Line} ->
            case parse_line(Line) of
                E = #entry{} ->
                    do_lines(File, Fun, Fun(E, Acc));
                undefined ->
                    do_lines(File, Fun, Acc)
            end;
        eof ->
            {ok, Acc};
        {error, Error} ->
            {error, Error}
    end.

-define(TS_Regex, "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}").

parse_line(Line) ->
    case re:run(Line, "^(" ?TS_Regex ") ([a-z]+) (.*)\n$", [{capture, all_but_first, list}]) of
        {match, [Timestamp, Tag, Text]} ->
            {ok, TS} = read_date(Timestamp),
            #entry{time = TS, tag = list_to_existing_atom(Tag), text = Text};
        nomatch ->
            undefined
    end.

read_date(Str) ->
    case io_lib:fread("~4d-~2d-~2dT~2d:~2d:~2d", Str) of
        {ok, [Year, Month, Day, Hour, Minute, Second], _} ->
            {ok, {{Year, Month, Day}, {Hour, Minute, Second}}};
        {error, Error} ->
            {error, Error}
    end.
