-module(rebar3_ebs_workspace).

-behaviour(ebs_workspace).

%% EBS Workspace
-export([ build_targets/1 ]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, rebar3_erlang_ls_agent).

%%==============================================================================
%% EBS Workspace
%%==============================================================================
-spec build_targets(ebs_workspace:build_targets_params()) ->
        ebs_workspace:build_targets_result().
build_targets(Params) ->
  gen_server:call(?SERVER, {workspace, build_targets, Params}).
