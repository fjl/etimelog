-module(etimelog).
-behaviour(application).
-export([start/2, stop/1]).

%% --------------------------------------------------------------------------------
%% -- application callbacks
start(_StartType, _StartArgs) ->
    Dir        = filename:join(os:getenv("HOME"), ".etimelog"),
    LogFile    = filename:join(Dir, "timelog.txt"),
    ConfigFile = filename:join(Dir, "etimelogrc"),

    filelib:ensure_dir(LogFile),

    case filelib:is_regular(ConfigFile) of
        true  -> read_config(ConfigFile);
        false -> write_defaults(ConfigFile)
    end,

    etimelog_sup:start_link(LogFile).

stop(_State) ->
    ok.

%% --------------------------------------------------------------------------------
%% -- helpers
read_config(ConfigFile) ->
    case file:consult(ConfigFile) of
        {ok, Terms} ->
            lists:foreach(fun ({Key, Value}) when is_atom(Key) ->
                                  application:set_env(etimelog, Key, Value);
                              (Term) ->
                                  error({config_file, {illegal_term, Term}})
                          end, Terms);
        {error, enoent} ->
            ok;
        {error, Error} when is_atom(Error) ->
            error({config_file, Error});
        {error, {Line, Mod, Info}} ->
            error_logger:error_msg("Error in ~/etimelog/.etimelogrc, line ~b:~s~n", [Line, Mod:format_error(Info)]),
            error({config_file, syntax})
    end.

write_defaults(ConfigFile) ->
    {ok, Defaults} = application:get_key(etimelog, env),
    {ok, File}     = file:open(ConfigFile, [write, exclusive]),
    lists:foreach(fun ({included_applications, []}) -> skip;
                      ({Key, Value})                -> io:format(File, "{~s, ~p}.~n", [Key, Value])
                  end, Defaults),
    file:close(File).
