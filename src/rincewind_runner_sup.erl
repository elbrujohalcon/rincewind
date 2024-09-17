-module(rincewind_runner_sup).

-behaviour(supervisor).

-type creation_error() :: {already_started, rincewind_runner:ref()} | invalid_wizard.

-export_type([creation_error/0]).

-export([start_link/0, start_runner/2]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, rincewind_runner_sup}, rincewind_runner_sup, #{}).

-spec start_runner(rincewind_wizard:name(), rincewind_runner:name()) ->
                      {ok, rincewind_runner:ref()} | {error, creation_error()}.
start_runner(WizardName, RunnerName) ->
    try rincewind:wizard(WizardName) of
        Wizard ->
            supervisor:start_child(rincewind_runner_sup, [Wizard, RunnerName])
    catch
        _:{noproc, _} ->
            {error, invalid_wizard}
    end.

-spec init(map()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(#{}) ->
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => 1000,
          period => 3600},

    Children =
        [#{id => rincewind_runner,
           start => {rincewind_runner, start_link, []},
           restart => transient,
           modules => [rincewind_runner]}],

    {ok, {SupFlags, Children}}.
