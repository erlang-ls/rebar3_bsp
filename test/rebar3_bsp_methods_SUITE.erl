-module(rebar3_bsp_methods_SUITE).

%% CT Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ build_initialize/1
        , build_initialized/1
        , workspace_buildtargets/1
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
  application:ensure_all_started(rebar3_bsp),
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  rebar3_bsp_agent:start_link(rebar_state:new()),
  Config.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  rebar3_bsp_agent:stop(),
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
  Result = rebar3_bsp_agent:handle_request(<<"build/initialize">>, #{}),
  ?assertEqual( #{ displayName => <<"rebar3_bsp">>
                 , bspVersion => <<"2.0.0">>
                 , capabilities => #{}
                 , version => <<"0.1.0">>
                 }, Result),
  ok.

-spec build_initialized(config()) -> ok.
build_initialized(_Config) ->
  Result = rebar3_bsp_agent:handle_request(<<"build/initialized">>, #{}),
  ?assertEqual(ok, Result),
  ok.

-spec workspace_buildtargets(config()) -> ok.
workspace_buildtargets(_Config) ->
  Result = rebar3_bsp_agent:handle_request(<<"workspace/buildTargets">>, #{}),
  ?assertEqual(#{ targets => [#{ id => <<"default">>}] }, Result),
  ok.
