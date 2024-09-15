-module(rincewind_phase).

-type name() :: atom().
-type init_arg() :: any().
-type definition() ::
    #{name := name(),
      callback_module := module(),
      init_arg => init_arg()}.
-type callback_state() :: any().
-type reason() :: any().
-type validation_error() :: any().
-type values() :: any().

-opaque t() ::
    #{name := name(),
      callback_module := module(),
      callback_state := callback_state()}.

-export_type([name/0, init_arg/0, definition/0, callback_state/0, reason/0,
              validation_error/0, values/0, t/0]).

-callback init(init_arg() | undefined) -> {ok, callback_state()} | {error, reason()}.
-callback validate(values(), callback_state()) -> valid | {invalid, validation_error()}.

-export([new/1]).
-export([name/1, validate/2]).

-spec new(definition()) -> t().
new(#{name := Name, callback_module := CallbackModule} = Definition) ->
    InitArg = maps:get(init_arg, Definition, undefined),
    case CallbackModule:init(InitArg) of
        {ok, InitState} ->
            #{name => Name,
              callback_module => CallbackModule,
              callback_state => InitState};
        {error, Reason} ->
            erlang:error({invalid_phase, #{definition => Definition, error => Reason}})
    end.

-spec name(t()) -> name().
name(#{name := Name}) ->
    Name.

-spec validate(t(), values()) -> valid | {invalid, validation_error()}.
validate(#{callback_module := Module, callback_state := State}, Values) ->
    Module:validate(Values, State).
