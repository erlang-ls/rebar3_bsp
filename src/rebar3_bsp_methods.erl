-module(rebar3_bsp_methods).

-export([ build_initialize/2
        , build_initialized/2
        , workspace_buildtargets/2
        , buildtarget_compile/2
        , buildtarget_sources/2
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

-type compileParams() :: #{ targets := [buildTargetIdentifier()]
                          , originId => binary()
                          , arguments => [binary()]
                          }.
-type compileResult() :: #{ originId => binary()
                          , statusCode := integer()
                          , dataKind := binary()
                          , data => any()
                          }.

-define(SOURCE_ITEM_KIND_FILE, 1).
-define(SOURCE_ITEM_KIND_DIR, 2).
-type buildTargetSourcesParams() :: #{ targets := [buildTargetIdentifier()] }.
-type sourceItemKind() :: ?SOURCE_ITEM_KIND_FILE
                        | ?SOURCE_ITEM_KIND_DIR.
-type sourceItem() :: #{ uri := uri()
                       , kind := sourceItemKind()
                       , generated := boolean()
                       }.
-type sourcesItem() :: #{ target := buildTargetIdentifier()
                        , sources := [sourceItem()]
                        , roots => [uri()]
                        }.
-type buildTargetSourcesResult() :: #{ items := [sourcesItem()] }.

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
build_initialized(#{}, State) ->
  {ok, _NreState} = rebar3:run(State, ["compile"]),
  ok.

-spec workspace_buildtargets(
        workspaceBuildTargetsParams(), rebar3_state:t()
       ) -> workspaceBuildTargetsResult().
workspace_buildtargets(#{}, State) ->
  Profiles = rebar_state:current_profiles(State),
  Targets = [#{id => atom_to_binary(P, utf8)} || P <- Profiles],
  #{ targets => Targets }.

-spec buildtarget_compile(compileParams(), rebar3_state:t()) -> compileResult().
buildtarget_compile(_Params, State) ->
  %% TODO: Hard-coded filename
  try rebar3:run(State, ["erl_subgraph_compile", "-f", "src/sample_app.erl"]) of
    {ok, _NewState} ->
      %% TODO: Compile test application and publish diagnostics
      #{ statusCode => 0 }
  catch C:E:S ->
      #{ class => C
       , exception => E
       , stacktrace => S
       }
  end.

%% TODO: Move types to header
%% TODO: Return new state in all functions
-spec buildtarget_sources(buildTargetSourcesParams(), rebar3_state:t()) ->
        buildTargetSourcesResult().
buildtarget_sources(#{ targets := Targets }, State) ->
  Apps = [A || A <- rebar_state:project_apps(State),
               lists:usort(to_atom(Targets)) =:= rebar_app_info:profiles(A)],
  Dirs = [rebar_app_info:dir(A) || A <- Apps],
  #{ items => Dirs }.

-spec version() -> binary().
version() ->
  {ok, Vsn} = application:get_key(rebar3_bsp, vsn),
  list_to_binary(Vsn).

-spec to_atom([binary()]) -> [atom()].
to_atom(Binaries) ->
  [binary_to_atom(B, utf8) || B <- Binaries].
