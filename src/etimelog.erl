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
