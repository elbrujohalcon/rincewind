-module(test_phase).

-behaviour(rincewind_phase).

-export([init/1, validate/2]).

-spec init(undefined) -> {ok, rincewind_phase:callback_state()};
          (binary()) -> {ok, rincewind_phase:callback_state()};
          (bad_init_arg) -> {error, bad_init_arg}.
init(undefined) ->
    {ok, phase_state};
init(<<PhaseTitle/binary>>) ->
    {ok, #{title => PhaseTitle}};
init(bad_init_arg) ->
    {error, bad_init_arg}.

-spec validate(binary(), #{title := binary()}) ->
                  valid | {invalid, #{expected := binary(), got := binary()}}.
validate(Value, #{title := Value}) ->
    valid;
validate(Value, #{title := Title}) ->
    {invalid, #{expected => Title, got => Value}}.
