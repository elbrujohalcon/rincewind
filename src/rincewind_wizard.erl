-module(rincewind_wizard).

-behaviour(gen_server).

-type name() :: atom().
-type definition() :: #{name := name(), phases := [rincewind_phase:definition(), ...]}.

-opaque t() :: #{name := name(), phases := [rincewind_phase:t(), ...]}.

-export_type([name/0, t/0, definition/0]).

-export([start_link/1, kill/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([definition/1]).
-export([name/1, phases/1]).

-spec name(t()) -> name().
name(#{name := Name}) ->
    Name.

-spec phases(t()) -> [rincewind_phase:t(), ...].
phases(#{phases := Phases}) ->
    Phases.

-spec start_link(definition()) -> {ok, pid()} | {error, term()}.
start_link(#{name := Name} = Definition) ->
    gen_server:start_link({local, Name}, rincewind_wizard, Definition, []).

-spec kill(name()) -> ok.
kill(WizardName) ->
    gen_server:stop(WizardName).

-spec definition(name()) -> t().
definition(WizardName) ->
    gen_server:call(WizardName, definition).

init(#{name := Name, phases := PhaseDefinitions} = Definition) ->
    try lists:map(fun rincewind_phase:new/1, PhaseDefinitions) of
        Phases ->
            {ok,
             #{name => Name,
               phases => Phases,
               definition => Definition}}
    catch
        error:Error ->
            {stop, Error}
    end.

handle_call(definition, _From, Wizard) ->
    {reply, Wizard, Wizard}.

handle_cast(Msg, Wizard) ->
    {stop, {unexpected_cast, Msg}, Wizard}.
