-module(rebar3_bsp_connection_SUITE).

%% CT Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ generate_discover/1
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
  {ok, Started} = application:ensure_all_started(rebar3_bsp),
  [{started, Started} | Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  [application:stop(App) || App <- ?config(started, Config)],
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  Config.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  ok.

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

%%==============================================================================
%% Testcases
%%==============================================================================
-spec generate_discover(config()) -> ok.
generate_discover(_Config) ->
  {ok, RootPath} = file:get_cwd(),
  rebar3_bsp_connection:generate(RootPath),
  {ok, Executable, Args, Env} = rebar3_bsp_connection:discover(RootPath),
  ?assertEqual("rebar3", filename:basename(Executable)),
  ?assertEqual(["bsp"], Args),
  ?assertEqual([{"QUIET", "1"}], Env),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
