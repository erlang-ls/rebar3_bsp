-module(rebar3_ebs_build_target).

-behaviour(ebs_build_target).

%% EBS Build Target
-export([ sources/1 ]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, rebar3_erlang_ls_agent).

%%==============================================================================
%% EBS Workspace
%%==============================================================================
-spec sources(ebs_build_target:sources_params()) ->
        ebs_build_target:sources_result().
sources(Params) ->
  gen_server:call(?SERVER, {build_target, sources, Params}).
