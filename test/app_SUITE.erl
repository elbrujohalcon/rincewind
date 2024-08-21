-module(app_SUITE).

-behaviour(ct_suite).

-export([all/0]).
-export([is_an_app/1]).

all() ->
    [is_an_app].

%% @doc Verifies that rincewind is a valid OTP app
is_an_app(_) ->
    {ok, Apps} = application:ensure_all_started(rincewind),
    [rincewind | _] = lists:reverse(Apps),
    ok = application:stop(rincewind).
