-module(rincewind).

-behaviour(application).

-export([start/2, stop/1]).

%% @private
-spec start(application:start_type(), map()) -> {ok, pid()}.
start(_StartType, #{}) ->
    rincewind_sup:start_link().

%% @private
-spec stop([]) -> ok.
stop([]) ->
    ok.
