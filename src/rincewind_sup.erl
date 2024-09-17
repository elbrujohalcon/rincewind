-module(rincewind_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, rincewind_sup}, rincewind_sup, #{}).

-spec init(map()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(#{}) ->
    Children =
        [#{id => rincewind_wizard_sup,
           start => {rincewind_wizard_sup, start_link, []},
           type => supervisor},
         #{id => rincewind_runner_sup,
           start => {rincewind_runner_sup, start_link, []},
           type => supervisor}],

    {ok, {#{}, Children}}.
