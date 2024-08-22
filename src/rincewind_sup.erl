-module(rincewind_sup).

-behaviour(supervisor).

-type creation_error() :: already_exists.

-export_type([creation_error/0]).

-export([start_link/0, start_wizard/1]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, rincewind_sup}, rincewind_sup, #{}).

-spec start_wizard(rincewind_wizard:definition()) -> ok | {error, creation_error()}.
start_wizard(Definition) ->
    case supervisor:start_child(rincewind_sup, [Definition]) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _}} ->
            {error, already_exists}
    end.

-spec init(map()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(#{}) ->
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => 1000,
          period => 3600},

    Children =
        [#{id => rincewind_wizard,
           start => {rincewind_wizard, start_link, []},
           restart => transient,
           modules => [rincewind_wizard]}],

    {ok, {SupFlags, Children}}.
