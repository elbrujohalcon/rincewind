-module(rincewind).

-behaviour(application).

-export([start/2, stop/1]).
-export([create_wizard/1, kill_wizard/1, wizard/1]).
-export([start_runner/2, current_phase/1, current_values/1, submit/2, skip_phase/1,
         jump_back/1, stop_runner/1]).

%% @private
-spec start(application:start_type(), map()) -> {ok, pid()}.
start(_StartType, #{}) ->
    rincewind_sup:start_link().

%% @private
-spec stop([]) -> ok.
stop([]) ->
    ok.

-spec create_wizard(rincewind_wizard:definition()) ->
                       ok | {error, rincewind_wizard_sup:creation_error()}.
create_wizard(Wizard) ->
    rincewind_wizard_sup:start_wizard(Wizard).

-spec kill_wizard(rincewind_wizard:name()) -> ok.
kill_wizard(WizardName) ->
    rincewind_wizard:kill(WizardName).

-spec wizard(rincewind_wizard:name()) -> rincewind_wizard:t().
wizard(WizardName) ->
    rincewind_wizard:definition(WizardName).

-spec start_runner(rincewind_wizard:name(), rincewind_runner:name()) ->
                      {ok, rincewind_runner:ref()} | {error, rincewind_runner_sup:creation_error()}.
start_runner(WizardName, RunnerName) ->
    rincewind_runner_sup:start_runner(WizardName, RunnerName).

-spec current_phase(rincewind_runner:ref()) -> done | rincewind_phase:t().
current_phase(RunnerRef) ->
    rincewind_runner:current_phase(RunnerRef).

-spec current_values(rincewind_runner:ref()) -> [rincewind_runner:result(), ...].
current_values(RunnerRef) ->
    rincewind_runner:current_values(RunnerRef).

-spec submit(rincewind_runner:ref(), rincewind_phase:values()) ->
                {invalid, rincewind_phase:validation_error()} |
                {error, done} |
                {next_phase, rincewind_phase:t()} |
                {done, [rincewind_runner:result(), ...]}.
submit(RunnerRef, Values) ->
    rincewind_runner:submit(RunnerRef, Values).

%% @todo Consider if there are phases that can't be skipped
%%       (Maybe unless they already have selected values)
-spec skip_phase(rincewind_runner:ref()) ->
                    {error, done} |
                    {next_phase, rincewind_phase:t()} |
                    {done, [rincewind_runner:result(), ...]}.
skip_phase(RunnerRef) ->
    rincewind_runner:skip_phase(RunnerRef).

%% @todo Consider if there are phases that can't jump back
%% @todo Consider jumping back and forth as many steps as desired
-spec jump_back(rincewind_runner:ref()) ->
                   {error, no_previous_phase} | {next_phase, rincewind_phase:t()}.
jump_back(RunnerRef) ->
    rincewind_runner:jump_back(RunnerRef).

-spec stop_runner(rincewind_runner:ref()) -> ok.
stop_runner(RunnerRef) ->
    rincewind_runner:stop(RunnerRef).
