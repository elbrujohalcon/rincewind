-module(rincewind).

-behaviour(application).

-export([start/2, stop/1]).
-export([create_wizard/1, kill_wizard/1, wizard/1]).

%% @private
-spec start(application:start_type(), map()) -> {ok, pid()}.
start(_StartType, #{}) ->
    rincewind_sup:start_link().

%% @private
-spec stop([]) -> ok.
stop([]) ->
    ok.

-spec create_wizard(rincewind_wizard:definition()) ->
                       ok | {error, rincewind_sup:creation_error()}.
create_wizard(Wizard) ->
    rincewind_sup:start_wizard(Wizard).

-spec kill_wizard(rincewind_wizard:name()) -> ok.
kill_wizard(WizardName) ->
    rincewind_wizard:kill(WizardName).

-spec wizard(rincewind_wizard:name()) -> rincewind_wizard:t().
wizard(WizardName) ->
    rincewind_wizard:definition(WizardName).
