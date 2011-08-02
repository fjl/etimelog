-module(etimelog_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, etimlog_sup).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init({}) ->
    File = timelog_file(),
    filelib:ensure_dir(File),

    FileWorker = ?CHILD(etimelog_file, [File], worker),

    {ok, {{one_for_one, 5, 10}, [FileWorker]}}.

timelog_file() ->
    filename:join([os:getenv("HOME"), ".etimelog", "time.log"]).
