-module(rebar3_bsp_methods).

-export([ build_initialize/2
        , build_initialized/2
        , workspace_buildtargets/2
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

-type workspaceBuildTargetsParams() :: #{}.
-type buildTargetIdentifier() :: #{ uri := uri() }.
-type buildTargetCapabilities() :: #{ canCompile := boolean()
                                    , canTest := boolean()
                                    , canRun := boolean()
                                    }.
-type buildTarget() :: #{ id := buildTargetIdentifier()
                        , displayName => binary()
                        , baseDirectory => uri()
                        , tags => [binary()]
                        , capabilities := buildTargetCapabilities()
                        , languageIds := [binary()]
                        , dependencies := [buildTargetIdentifier()]
                        , dataKind => binary()
                        , data => any()
                        }.
-type workspaceBuildTargetsResult() :: #{ targets => [buildTarget()]}.
-define(BSP_VSN, <<"2.0.0">>).

-spec build_initialize(initializeBuildParams(), rebar3_state:t()) ->
        initializeBuildResult().
build_initialize(_Params, _State) ->
  #{ displayName => <<"rebar3_bsp">>
   , version => version()
   , bspVersion => ?BSP_VSN
   , capabilities => #{}
   }.

-spec build_initialized(initializedBuildParams(), rebar3_state:t()) -> ok.
build_initialized(#{}, _State) ->
  ok.

-spec workspace_buildtargets(
        workspaceBuildTargetsParams(), rebar3_state:t()
       ) -> workspaceBuildTargetsResult().
workspace_buildtargets(#{}, State) ->
  Profiles = rebar_state:current_profiles(State),
  Targets = [#{id => atom_to_binary(P, utf8)} || P <- Profiles],
  #{ targets => Targets }.

-spec version() -> binary().
version() ->
  {ok, Vsn} = application:get_key(rebar3_bsp, vsn),
  list_to_binary(Vsn).
