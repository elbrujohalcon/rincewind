-module(wizard_definition_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([minimal/1, duplicated/1, bad_phase/1, complete_coverage/1]).

all() ->
    [minimal, duplicated, bad_phase, complete_coverage].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(rincewind),
    Config.

end_per_suite(_) ->
    ok = application:stop(rincewind),
    ok.

minimal(_) ->
    ok =
        rincewind:create_wizard(#{name => minimal,
                                  phases =>
                                      [#{name => minimal_phase, callback_module => test_phase}]}),

    %% You can inspect the wizard with the functions from rincewind_wizard
    MinimalWizard = rincewind:wizard(minimal),

    minimal = rincewind_wizard:name(MinimalWizard),
    [_] = rincewind_wizard:phases(MinimalWizard),
    ok.

%% @doc There cannot be two wizards with the same name
duplicated(_) ->
    Definition =
        #{name => duplicated, phases => [#{name => duplicated, callback_module => test_phase}]},
    ok = rincewind:create_wizard(Definition),
    {error, already_exists} = rincewind:create_wizard(Definition),
    ok = rincewind:kill_wizard(duplicated),
    ok = rincewind:create_wizard(Definition),
    ok.

%% @doc Notice that test_phase:init/1 returns {error, bad_init_arg} if it receives bad_init_arg.
bad_phase(_) ->
    {error, {invalid_phase, #{error := bad_init_arg}}} =
        rincewind:create_wizard(#{name => bad_phase,
                                  phases =>
                                      [#{name => bad,
                                         callback_module => test_phase,
                                         init_arg => bad_init_arg}]}),
    ok.

complete_coverage(_) ->
    ok =
        rincewind:create_wizard(#{name => coverage,
                                  phases => [#{name => coverage, callback_module => test_phase}]}),
    ok = gen_server:cast(coverage, poison),
    %% The wizard died from the poison
    try rincewind:wizard(coverage) of
        Wizard ->
            ct:fail("coverage should not be alive: ~p", [Wizard])
    catch
        exit:Reason ->
            {{unexpected_cast, poison}, _} = Reason
    end.
