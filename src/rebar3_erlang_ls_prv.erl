-module(rebar3_erlang_ls_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, erlang_ls).
-define(DEPS, [compile]).
-define(AGENT, rebar3_erlang_ls_agent).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
                               {name, ?PROVIDER},
                               {module, ?MODULE},
                               {bare, true},
                               {deps, ?DEPS},
                               {example, "rebar3 erlang_ls"},
                               {opts, []},
                               {short_desc, "Erlang LS plugin for rebar3"},
                               {desc, "Interact with the Erlang LS Language Server"},
                               {profiles, [test]}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  setup_name(State),
  start_agent(State),
  {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

-spec start_agent(rebar_state:t()) -> no_return().
start_agent(State) ->
  simulate_proc_lib(),
  true = register(?AGENT, self()),
  {ok, GenState} = rebar3_erlang_ls_agent:init(State),
  gen_server:enter_loop(rebar3_erlang_ls_agent, [], GenState, {local, ?AGENT}, hibernate).

-spec setup_name(rebar_state:t()) -> ok.
setup_name(State) ->
  {_Long, Short, Opts} = rebar_dist_utils:find_options(State),
  Name = case Short of
           undefined ->
             list_to_atom(filename:basename(rebar_state:dir(State)));
           N ->
             N
         end,
  rebar_dist_utils:short(Name, Opts),
  ok.

-spec simulate_proc_lib() -> ok.
simulate_proc_lib() ->
  FakeParent = spawn_link(fun() -> timer:sleep(infinity) end),
  put('$ancestors', [FakeParent]),
  put('$initial_call', {erlang_ls_agent, init, 1}),
  ok.
