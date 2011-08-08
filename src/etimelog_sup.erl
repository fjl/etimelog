-module(etimelog_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

-define(SERVER, etimelog_sup).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link(TimelogFile) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {TimelogFile}).

init({TimelogFile}) ->
    FileWorker = ?CHILD(etimelog_file, [TimelogFile], worker),

    {ok, {{one_for_one, 5, 10}, [FileWorker]}}.
