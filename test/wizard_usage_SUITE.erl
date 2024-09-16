-module(wizard_usage_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         end_per_suite/1]).
-export([invalid_wizard/1, start_stop/1, first_phase/1, submit_values/1, skip_phase/1,
         jump_back/1, complete_coverage/1]).

all() ->
    [invalid_wizard,
     start_stop,
     first_phase,
     submit_values,
     skip_phase,
     jump_back,
     complete_coverage].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(rincewind),
    Config.

init_per_testcase(invalid_wizard, Config) ->
    %% No wizard needs to be created for this test
    Config;
init_per_testcase(TestCase, Config)
    when TestCase == start_stop; TestCase == complete_coverage ->
    %% For start_stop/1 and complete_coverage/1 we just need a wizard.
    %% The actual phases are not important.
    ok =
        rincewind:create_wizard(#{name => TestCase,
                                  phases =>
                                      [#{name => test_phase, callback_module => test_phase}]}),
    Config;
init_per_testcase(TestCase, Config) ->
    %% For other tests we need a fully functional wizard, with three phases
    %% To check the implementation of each phase (they're all similar anyway, check test_phase)
    ok =
        rincewind:create_wizard(#{name => TestCase,
                                  phases =>
                                      [#{name => first_phase,
                                         callback_module => test_phase,
                                         init_arg => <<"First">>},
                                       #{name => second_phase,
                                         callback_module => test_phase,
                                         init_arg => <<"Second">>},
                                       #{name => third_phase,
                                         callback_module => test_phase,
                                         init_arg => <<"Third">>}]}),
    Config.

end_per_testcase(invalid_wizard, Config) ->
    Config;
end_per_testcase(TestCase, Config) ->
    ok = rincewind:kill_wizard(TestCase),
    Config.

end_per_suite(_) ->
    ok = application:stop(rincewind),
    ok.

invalid_wizard(_) ->
    {error, invalid_wizard} = rincewind:start_runner(invalid_wizard, user_id).

start_stop(_) ->
    ct:log("~p", [supervisor:which_children(rincewind_sup)]),
    {ok, WizardProcess} = rincewind:start_runner(start_stop, user_id),
    %% We allow a single process for user
    {error, {already_started, WizardProcess}} = rincewind:start_runner(start_stop, user_id),
    ok = rincewind:stop_runner(WizardProcess),
    {ok, NewWizardProcess} = rincewind:start_runner(start_stop, user_id),
    ok = rincewind:stop_runner(NewWizardProcess).

first_phase(_) ->
    {ok, WizardProcess} = rincewind:start_runner(first_phase, user_id),
    %% The runner just started, the user should see the first phase as the current phase
    FirstPhase = rincewind:current_phase(WizardProcess),
    first_phase = rincewind_phase:name(FirstPhase),
    ok.

submit_values(_) ->
    {ok, WizardProcess} = rincewind:start_runner(submit_values, user_id),
    FirstPhase = rincewind:current_phase(WizardProcess),
    first_phase = rincewind_phase:name(FirstPhase),
    %% Check test_phase implemenation of rincewind_phase callbacks to understand these results
    {invalid, #{expected := <<"First">>, got := <<"Second">>}} =
        rincewind:submit(WizardProcess, <<"Second">>),
    {next_phase, SecondPhase} = rincewind:submit(WizardProcess, <<"First">>),
    SecondPhase = rincewind:current_phase(WizardProcess),
    second_phase = rincewind_phase:name(SecondPhase),
    {next_phase, ThirdPhase} = rincewind:submit(WizardProcess, <<"Second">>),
    third_phase = rincewind_phase:name(ThirdPhase),
    {done,
     [#{phase := FirstPhase, values := <<"First">>},
      #{phase := SecondPhase, values := <<"Second">>},
      #{phase := ThirdPhase, values := <<"Third">>}]} =
        rincewind:submit(WizardProcess, <<"Third">>),
    {error, done} = rincewind:submit(WizardProcess, <<"Fourth">>),
    done = rincewind:current_phase(WizardProcess).

skip_phase(_) ->
    {ok, WizardProcess} = rincewind:start_runner(skip_phase, user_id),
    [#{phase := _} = FirstResult,
     #{phase := SecondPhase} = SecondResult,
     #{phase := ThirdPhase} = ThirdResult] =
        rincewind:current_values(WizardProcess),
    false = maps:is_key(values, FirstResult),
    false = maps:is_key(values, SecondResult),
    false = maps:is_key(values, ThirdResult),

    {next_phase, SecondPhase} = rincewind:skip_phase(WizardProcess),
    [FirstResult, SecondResult, ThirdResult] = rincewind:current_values(WizardProcess),

    SecondPhase = rincewind:current_phase(WizardProcess),
    {next_phase, ThirdPhase} = rincewind:submit(WizardProcess, <<"Second">>),
    [FirstResult,
     #{phase := SecondPhase, values := <<"Second">>} = SecondResultWithValues,
     ThirdResult] =
        rincewind:current_values(WizardProcess),

    {done, [FirstResult, SecondResultWithValues, ThirdResult]} =
        rincewind:skip_phase(WizardProcess),
    {error, done} = rincewind:skip_phase(WizardProcess),
    done = rincewind:current_phase(WizardProcess).

jump_back(_) ->
    {ok, WizardProcess} = rincewind:start_runner(jump_back, user_id),
    [#{phase := FirstPhase}, #{phase := SecondPhase}, #{phase := ThirdPhase}] =
        AllEmpty = rincewind:current_values(WizardProcess),
    [] = [Result || Result <- AllEmpty, maps:is_key(values, Result)],

    {next_phase, SecondPhase} = rincewind:skip_phase(WizardProcess),
    AllEmpty = rincewind:current_values(WizardProcess),

    {next_phase, FirstPhase} = rincewind:jump_back(WizardProcess),
    AllEmpty = rincewind:current_values(WizardProcess),

    {error, no_previous_phase} = rincewind:jump_back(WizardProcess),
    {next_phase, SecondPhase} = rincewind:submit(WizardProcess, <<"First">>),
    [#{phase := FirstPhase, values := <<"First">>} = FirstFull, SecondEmpty, ThirdEmpty] =
        rincewind:current_values(WizardProcess),
    false = maps:is_key(values, SecondEmpty),
    false = maps:is_key(values, ThirdEmpty),

    {next_phase, ThirdPhase} = rincewind:submit(WizardProcess, <<"Second !">>),
    [FirstFull, #{phase := SecondPhase, values := <<"Second !">>}, ThirdEmpty] =
        rincewind:current_values(WizardProcess),

    %% Previously selected values are not lost when jumping back
    {next_phase, SecondPhase} = rincewind:jump_back(WizardProcess),
    [FirstFull, #{phase := SecondPhase, values := <<"Second !">>} = SecondFull, ThirdEmpty] =
        rincewind:current_values(WizardProcess),

    %% They can be replaced, tho
    {next_phase, FirstPhase} = rincewind:jump_back(WizardProcess),
    {next_phase, SecondPhase} = rincewind:submit(WizardProcess, <<"First, too">>),
    [#{phase := FirstPhase, values := <<"First, too">>}, SecondFull, ThirdEmpty] =
        FinalResult = rincewind:current_values(WizardProcess),

    %% And they're preserved even when skipping the phase
    {next_phase, ThirdPhase} = rincewind:skip_phase(WizardProcess),
    FinalResult = rincewind:current_values(WizardProcess),

    {done, FinalResult} = rincewind:skip_phase(WizardProcess),
    {next_phase, ThirdPhase} = rincewind:jump_back(WizardProcess),
    FinalResult = rincewind:current_values(WizardProcess),
    ok.

complete_coverage(_) ->
    {ok, WizardProcess1} = rincewind:start_runner(complete_coverage, user_id),
    ok = gen_server:cast(WizardProcess1, poison),
    %% The runner died from the poison
    try sys:get_status(WizardProcess1) of
        Status ->
            ct:fail("~p should not be alive: ~p", [WizardProcess1, Status])
    catch
        _:Reason ->
            {{unexpected_cast, poison}, _} = Reason
    end.
