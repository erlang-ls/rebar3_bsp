-module(rebar3_bsp_build_target).

-behaviour(ebs_build_target).

%% EBS Build Target
-export([ sources/1 ]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, rebar3_bsp_agent).

%%==============================================================================
%% EBS Workspace
%%==============================================================================
-spec sources(ebs_build_target:sources_params()) ->
        ebs_build_target:sources_result().
sources(Params) ->
  gen_server:call(?SERVER, {build_target, sources, Params}).
