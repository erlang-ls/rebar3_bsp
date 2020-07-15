-module(rebar3_bsp_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, bsp).
-define(DEPS, [compile]).
-define(AGENT, rebar3_bsp_agent).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Opts = [{generate, undefined, "generate", boolean, "Generate the BSP Connection file and exit"}],
  Provider = providers:create([
                               {name, ?PROVIDER},
                               {module, ?MODULE},
                               {bare, true},
                               {deps, ?DEPS},
                               {example, "rebar3 bsp"},
                               {opts, Opts},
                               {short_desc, "Build Server Protocol (BSP) plugin"},
                               {desc, "Plugin adding Build Server Protocol (BSP) support for rebar3"},
                               {profiles, [test]}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  {Args, _} = rebar_state:command_parsed_args(State),
  case proplists:is_defined(generate, Args) of
    true ->
      Dir = rebar_state:dir(State),
      rebar3_bsp_connection:generate(Dir);
    false ->
      %% TODO: Distribution only needed for debugging
      setup_name(State),
      start_agent(State)
  end,
  {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

-spec start_agent(rebar_state:t()) -> no_return().
start_agent(State) ->
  simulate_proc_lib(),
  true = register(?AGENT, self()),
  rebar_log:log(info, "Starting BSP group leader...", []),
  rebar3_bsp_stdio:start_link(),
  simulate_group_leader(),
  {ok, GenState} = rebar3_bsp_agent:init(State),
  gen_server:enter_loop(rebar3_bsp_agent, [], GenState, {local, ?AGENT}, hibernate).

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
  put('$initial_call', {rebar3_bsp_agent, init, 1}),
  ok.

-spec simulate_group_leader() -> ok.
simulate_group_leader() ->
  Pid = spawn_link(fun noop_group_leader/0),
  erlang:group_leader(Pid, self()).

-spec noop_group_leader() -> no_return().
noop_group_leader() ->
  receive
    Message ->
      rebar_log:log(debug, "Fake group leader intercepted output [message=~p]", [Message]),
      case Message of
        {io_request, From, ReplyAs, getopts} ->
          From ! {io_reply, ReplyAs, []};
        {io_request, From, ReplyAs, _} ->
          From ! {io_reply, ReplyAs, ok};
        _ ->
          ok
      end,
      noop_group_leader()
  end.
