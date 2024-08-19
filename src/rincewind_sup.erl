-module(rincewind_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, rincewind_sup}, rincewind_sup, #{}).

-spec init(map()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(#{}) ->
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => 1000,
          period => 3600},

    Children =
        [#{id => rincewind_wizard,
           start => {rincewind_wizard, start_link, []},
           modules => [rincewind_wizard]}],

    {ok, {SupFlags, Children}}.