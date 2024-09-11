-module(rincewind_runner).

-behaviour(gen_server).

-type name() :: atom().

-opaque ref() :: pid().
-opaque t() ::
    #{name := name(),
      wizard := rincewind_wizard:t(),
      phase := rincewind_phase:t()}.

-export_type([name/0, ref/0, t/0]).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([current_phase/1]).

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

-spec current_phase(ref()) -> rincewind_phase:t().
current_phase(RunnerPid) ->
    gen_server:call(RunnerPid, current_phase).

-spec stop(ref()) -> ok.
stop(RunnerPid) ->
    gen_server:stop(RunnerPid).

init(#{name := Name, wizard := Wizard}) ->
    {ok,
     #{name => Name,
       wizard => Wizard,
       phase => hd(rincewind_wizard:phases(Wizard))}}.

handle_call(current_phase, _From, #{phase := CurrentPhase} = Runner) ->
    {reply, CurrentPhase, Runner}.

handle_cast(Msg, Runner) ->
    {stop, {unexpected_cast, Msg}, Runner}.
