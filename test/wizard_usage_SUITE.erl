-module(wizard_usage_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         end_per_suite/1]).
-export([invalid_wizard/1, start_stop/1, first_phase/1, complete_coverage/1]).

all() ->
    [invalid_wizard, start_stop, first_phase, complete_coverage].

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
