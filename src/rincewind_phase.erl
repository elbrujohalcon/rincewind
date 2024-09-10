-module(rincewind_phase).

-type name() :: atom().
-type init_arg() :: any().
-type definition() ::
    #{name := name(),
      callback_module := module(),
      init_arg => init_arg()}.
-type callback_state() :: any().
-type reason() :: any().

-opaque t() ::
    #{name := name(),
      callback_module := module(),
      callback_state := callback_state()}.

-export_type([name/0, init_arg/0, definition/0, callback_state/0, reason/0, t/0]).

-callback init(init_arg() | undefined) -> {ok, callback_state()} | {error, reason()}.

-export([new/1]).

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
