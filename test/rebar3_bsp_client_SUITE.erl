-module(rebar3_bsp_client_SUITE).

%% CT Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ build_initialize/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  %% Required for the application environment to be loaded
  application:load(rebar3_bsp),
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  file:set_cwd(?config(cwd, Config)),
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  {ok, Cwd} = file:get_cwd(),
  RootPath = sample_app_dir(),
  ok = file:set_cwd(RootPath),
  %% Trigger an explicit compilation, so that the pre_compile hook is executed
  %% and a symlink is created for the plugin, pointing to the same repo
  %% TODO: Find a simpler solution
  os:cmd("rebar3 compile"),
  %% TODO: CT is leaking processes
  {ok, _} = rebar3_bsp_client:start_link(RootPath),
  [{cwd, Cwd} | Config].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  rebar3_bsp_client:stop(),
  ok.

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

%%==============================================================================
%% Testcases
%%==============================================================================
-spec build_initialize(config()) -> ok.
build_initialize(_Config) ->
  Result = rebar3_bsp_client:build_initialize(#{}),
  Expected = #{ id => 1
              , jsonrpc => <<"2.0">>
              , result =>
                  #{ bspVersion => <<"2.0.0">>
                   , capabilities => #{}
                   , displayName => <<"rebar3_bsp">>
                   , version => <<"0.1.0">>
                   }},
  ?assertEqual(Expected, Result),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec sample_app_dir() -> file:filename().
sample_app_dir() ->
  filename:join([code:priv_dir(rebar3_bsp), "sample"]).
