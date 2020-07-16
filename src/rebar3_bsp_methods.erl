-module(rebar3_bsp_methods).

-export([ build_initialize/1
        , build_initialized/1
        ]).

-type uri() :: binary().
-type buildClientCapabilities() :: #{ languageIds := [binary()] }.
-type initializeBuildParams() :: #{ displayName := binary()
                                  , version := binary()
                                  , bspVersion := binary()
                                  , rootUri := uri()
                                  , capabilities := buildClientCapabilities()
                                  , data => any()
                                  }.
-type compileProvider() :: #{ languageIds := [binary()] }.
-type testProvider() :: #{ languageIds := [binary()] }.
-type runProvider() :: #{ languageIds := [binary()] }.
-type buildServerCapabilities() :: #{ compileProvider => compileProvider()
                                    , testProvider => testProvider()
                                    , runProvider => runProvider()
                                    , inverseSourcesProvider => boolean()
                                    , dependencySourcesProvider => boolean()
                                    , resourcesProvider => boolean()
                                    , buildTargetChangedProvider => boolean()
                                    }.
-type initializeBuildResult() :: #{ displayName := binary()
                                  , version := binary()
                                  , bspVersion := binary()
                                  , capabilities := buildServerCapabilities()
                                  , data => any()
                                  }.

-type initializedBuildParams() :: #{}.

-define(BSP_VSN, <<"2.0.0">>).

-spec build_initialize(initializeBuildParams()) -> initializeBuildResult().
build_initialize(_Params) ->
  #{ displayName => <<"rebar3_bsp">>
   , version => version()
   , bspVersion => ?BSP_VSN
   , capabilities => #{}
   }.

-spec build_initialized(initializedBuildParams()) -> ok.
build_initialized(#{}) ->
  ok.

-spec version() -> binary().
version() ->
  {ok, Vsn} = application:get_key(rebar3_bsp, vsn),
  list_to_binary(Vsn).
