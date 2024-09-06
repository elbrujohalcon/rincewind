-module(wizard_definition_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([minimal/1, duplicated/1, complete_coverage/1]).

all() ->
    [minimal, duplicated, complete_coverage].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(rincewind),
    Config.

end_per_suite(_) ->
    ok = application:stop(rincewind),
    ok.

minimal(_) ->
    ok = rincewind:create_wizard(#{name => minimal, phases => [#{name => minimal_phase}]}),

    %% You can inspect the wizard with the functions from rincewind_wizard
    MinimalWizard = rincewind:wizard(minimal),

    minimal = rincewind_wizard:name(MinimalWizard),
    [_] = rincewind_wizard:phases(MinimalWizard),
    ok.

%% @doc There cannot be two wizards with the same name
duplicated(_) ->
    Definition = #{name => duplicated, phases => [#{name => duplicated}]},
    ok = rincewind:create_wizard(Definition),
    {error, already_exists} = rincewind:create_wizard(Definition),
    ok = rincewind:kill_wizard(duplicated),
    ok = rincewind:create_wizard(Definition),
    ok.

complete_coverage(_) ->
    ok = rincewind:create_wizard(#{name => coverage, phases => [#{name => coverage}]}),
    ok = gen_server:cast(coverage, poison),
    %% The wizard died from the poison
    try rincewind:wizard(coverage) of
        Wizard ->
            ct:fail("coverage should not be alive: ~p", [Wizard])
    catch
        exit:Reason ->
            {{unexpected_cast, poison}, _} = Reason
    end.
