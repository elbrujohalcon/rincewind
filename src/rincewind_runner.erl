-module(rincewind_runner).

-behaviour(gen_server).

-type name() :: atom().

-opaque ref() :: pid().

-type result() :: #{phase := rincewind_phase:t(), values => rincewind_phase:values()}.

-opaque t() ::
    #{name := name(),
      wizard := rincewind_wizard:t(),
      phase_number := pos_integer() | done,
      values := [not_chosen | {chosen, rincewind_phase:values()}]}.

-export_type([name/0, ref/0, result/0, t/0]).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([current_phase/1, current_values/1, submit/2, skip_phase/1]).

-spec start_link(rincewind_wizard:t(), name()) ->
                    {ok, ref()} | {error, rincewind_runner_sup:creation_error()}.
start_link(Wizard, RunnerName) ->
    ProcessName =
        binary_to_atom(iolist_to_binary(io_lib:format("rincewind_runner-~p-~p",
                                                      [rincewind_wizard:name(Wizard),
                                                       RunnerName]))),
    gen_server:start_link({local, ProcessName},
                          rincewind_runner,
                          #{wizard => Wizard, name => RunnerName},
                          []).

-spec current_phase(ref()) -> done | rincewind_phase:t().
current_phase(RunnerPid) ->
    gen_server:call(RunnerPid, current_phase).

-spec current_values(ref()) -> [result(), ...].
current_values(RunnerRef) ->
    gen_server:call(RunnerRef, current_values).

-spec submit(ref(), rincewind_phase:values()) ->
                {invalid, rincewind_phase:validation_error()} |
                {error, done} |
                {next_phase, rincewind_phase:t()} |
                {done, [result(), ...]}.
submit(RunnerRef, Values) ->
    gen_server:call(RunnerRef, {submit, Values}).

-spec skip_phase(ref()) ->
                    {error, done} | {next_phase, rincewind_phase:t()} | {done, [result(), ...]}.
skip_phase(RunnerRef) ->
    gen_server:call(RunnerRef, skip_phase).

-spec stop(ref()) -> ok.
stop(RunnerPid) ->
    gen_server:stop(RunnerPid).

init(#{name := Name, wizard := Wizard}) ->
    {ok,
     #{name => Name,
       wizard => Wizard,
       values => lists:duplicate(length(rincewind_wizard:phases(Wizard)), not_chosen),
       phase_number => 1}}.

handle_call(current_phase, _From, #{phase_number := done} = Runner) ->
    {reply, done, Runner};
handle_call(current_phase,
            _From,
            #{phase_number := CurrentPhase, wizard := Wizard} = Runner) ->
    {reply, lists:nth(CurrentPhase, rincewind_wizard:phases(Wizard)), Runner};
handle_call(current_values, _From, #{wizard := Wizard, values := Values} = Runner) ->
    Result = lists:zipwith(fun build_result/2, rincewind_wizard:phases(Wizard), Values),
    {reply, Result, Runner};
handle_call(skip_phase, _From, #{phase_number := done} = Runner) ->
    {reply, {error, done}, Runner};
handle_call(skip_phase,
            _From,
            #{phase_number := CurrentPhaseNumber,
              values := Values,
              wizard := Wizard} =
                Runner) ->
    case rincewind_wizard:phases(Wizard) of
        WizardPhases
            when length(WizardPhases) == CurrentPhaseNumber -> %% this was the last one
            Result = lists:zipwith(fun build_result/2, rincewind_wizard:phases(Wizard), Values),
            {reply, {done, Result}, Runner#{phase_number := done}};
        WizardPhases ->
            {reply,
             {next_phase, lists:nth(CurrentPhaseNumber + 1, WizardPhases)},
             Runner#{phase_number := CurrentPhaseNumber + 1}}
    end;
handle_call({submit, _}, _From, #{phase_number := done} = Runner) ->
    {reply, {error, done}, Runner};
handle_call({submit, SelectedValues},
            _From,
            #{phase_number := CurrentPhaseNumber,
              values := Values,
              wizard := Wizard} =
                Runner) ->
    WizardPhases = rincewind_wizard:phases(Wizard),
    CurrentPhase = lists:nth(CurrentPhaseNumber, WizardPhases),
    %% @todo Consider at some point, the need to pass the current value,
    %%       or the values of the previous phases,
    %%       or even the entire wizard to this function.
    %%       Also worth considering if the phase would like to transform
    %%       the selected values (e.g., it receives numbers and returns strings)
    case rincewind_phase:validate(CurrentPhase, SelectedValues) of
        valid ->
            {UpToCurrentPhase, NextPhases} = lists:split(CurrentPhaseNumber, Values),
            [_ | PreviousPhases] = lists:reverse(UpToCurrentPhase),
            NewValues = lists:reverse(PreviousPhases) ++ [{chosen, SelectedValues} | NextPhases],
            case length(WizardPhases) of
                CurrentPhaseNumber -> %% this was the last one
                    Result =
                        lists:zipwith(fun build_result/2,
                                      rincewind_wizard:phases(Wizard),
                                      NewValues),
                    {reply, {done, Result}, Runner#{phase_number := done, values := NewValues}};
                _ ->
                    {reply,
                     {next_phase, lists:nth(CurrentPhaseNumber + 1, WizardPhases)},
                     Runner#{phase_number := CurrentPhaseNumber + 1, values := NewValues}}
            end;
        {invalid, ValidationError} ->
            {reply, {invalid, ValidationError}, Runner}
    end.

handle_cast(Msg, Runner) ->
    {stop, {unexpected_cast, Msg}, Runner}.

build_result(Phase, not_chosen) ->
    #{phase => Phase};
build_result(Phase, {chosen, Values}) ->
    #{phase => Phase, values => Values}.
