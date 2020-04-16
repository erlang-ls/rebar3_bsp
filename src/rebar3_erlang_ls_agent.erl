%%==============================================================================
%% A stripped down version of rebar3's rebar_agent
%%
%% Runs a process that holds a rebar3 state and can be used to
%% statefully maintain loaded project state into a running VM.
%%==============================================================================
-module(rebar3_erlang_ls_agent).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================

-export([ start_link/1
        , run_ct_test/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #{ rebar3_state := rebar3_state:t() }.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link(rebar3_state:t()) -> {ok, pid()}.
start_link(R3State) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, R3State, []).

-spec run_ct_test(atom(), atom()) -> ok.
run_ct_test(Suite, Case) ->
  Groups = string:join([atom_to_list(G) || G <- ct_groups(Suite, Case)], ","),
  Args = [ io_lib:format("--suite=~s", [Suite])
         , io_lib:format("--case=~s", [Case])
         , io_lib:format("--group=~s", [Groups])
         ],
  gen_server:call(?SERVER, {run_ct_test, Args}, infinity).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init(rebar3_state:t()) -> {ok, state()}.
init(R3State) ->
  rebar_log:log(debug, "Starting Erlang LS Agent...~n", []),
  {ok, #{ rebar3_state => R3State }}.

-spec handle_call(any(), any(), state()) ->
        {reply, any(), state()} | {noreply, state()}.
handle_call({run_ct_test, Args}, From, State) ->
  #{ rebar3_state := R3State } = State,
  R3State1 = update_state_for_ct(R3State, From),
  %% For some reason we have to explicitly invoke compilation
  rebar_log:log(debug, "Compiling code for CT", []),
  try rebar3:run(R3State1, ["compile"]) of
    {ok, _} ->
      rebar_log:log(debug, "Running CT test ~p", [Args]),
      try rebar3:run(R3State1, ["ct"|Args]) of
        {ok, _} ->
          %% rebar3 unset the paths on success, causing the second run to fail.
          %% Let's revert that.
          %% TODO: Report and link issue
          ok = rebar_paths:set_paths([deps, plugins], R3State1),
          {reply, ok, State#{ rebar3_state => R3State1 }};
        {error, Error} ->
          rebar_log:log(debug, "Running CT tests failed: ~p", [Error]),
          %% The CT Hook will take care of returning a more meaningful error
          {noreply, State#{ rebar3_state => R3State1 }}
      catch C:E:S ->
          rebar_log:log(debug, "Running CT failed", []),
          {reply, {error, {C, E, S}}, State#{ rebar3_state => R3State1 }}
      end
  catch C:E:S ->
      rebar_log:log(debug, "Compilation for CT failed", []),
      {reply, {error, {C, E, S}}, State#{ rebar3_state => R3State1 }}
  end.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({result, From, Result}, State) ->
  rebar_log:log(debug, "Received result: ~p", [Result]),
  gen_server:reply(binary_to_term(From), Result),
  {noreply, State};
handle_info({'DOWN', _Ref, process, _Pid, normal}, State) ->
  {noreply, State};
handle_info(Request, State) ->
  rebar_log:log(debug, "Unexpected handle_info request: ~p", [Request]),
  {noreply, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec update_state_for_ct(rebar_state:t(), {pid(), reference()}) ->
        rebar_state:t().
update_state_for_ct(State, From) ->
  disable_cover(inject_ct_hook(State, From)).

-spec disable_cover(rebar_state:t()) -> rebar_state:t().
disable_cover(State) ->
  rebar_state:set(State, cover_enabled, false).

-spec inject_ct_hook(rebar_state:t(), {pid(), reference()}) -> rebar_state:t().
inject_ct_hook(State, From) ->
  Opts0 = rebar_state:get(State, ct_opts, []),
  Hooks0 = proplists:get_value(ct_hooks, Opts0, []),
  Hook = {rebar3_erlang_ls_ct_hook, #{from => term_to_binary(From)}},
  Hooks = lists:keystore(rebar3_erlang_ls_ct_hook, 1, Hooks0, Hook),
  Opts = lists:keystore(ct_hooks, 1, Opts0, {ct_hooks, Hooks}),
  rebar_state:set(State, ct_opts, Opts).

-spec ct_groups(atom(), atom()) -> [atom()].
ct_groups(Suite, Case) ->
  case erlang:function_exported(Suite, groups, 0) of
    true ->
      [Group || {Group, _Prop, Cases} <-
                  Suite:groups(), lists:member(Case, Cases)];
    false ->
      []
  end.
