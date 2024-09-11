-module(rincewind_runner).

-behaviour(gen_server).

-type name() :: atom().

-opaque ref() :: pid().
-opaque t() :: #{name := name(), wizard := rincewind_wizard:t()}.

-export_type([name/0, ref/0, t/0]).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-spec start_link(rincewind_wizard:t(), name()) ->
                    {ok, ref()} | {error, rincewind_runner_sup:creation_error()}.
start_link(Wizard, RunnerName) ->
    ProcessName =
        binary_to_atom(iolist_to_binary(io_lib:format("rincewind_runner-~p-~p",
                                                      [rincewind_wizard:name(Wizard),
                                                       RunnerName]))),
    gen_server:start_link({local, ProcessName},
                          rincewind_runner,
                          #{wizard => Wizard, name => RunnerName},
                          []).

-spec stop(ref()) -> ok.
stop(RunnerPid) ->
    gen_server:stop(RunnerPid).

init(#{name := Name, wizard := Wizard}) ->
    {ok, #{name => Name, wizard => Wizard}}.

handle_call(Msg, _From, Runner) ->
    {stop, {unexpected_call, Msg}, Runner}.

handle_cast(Msg, Runner) ->
    {stop, {unexpected_cast, Msg}, Runner}.
