%%==============================================================================
%% BSP Connection
%%==============================================================================
-module(rebar3_bsp_connection).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ generate/1
        , discover/1
        , exists/1
        ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type details() :: #{ name => binary()
                    , version => binary()
                    , bspVersion => binary()
                    , languages => [binary()]
                    , argv => [binary()]
                    }.
-export_type([details/0]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(BSP_VSN   , <<"2.0.0">>).
-define(NAME      , <<"rebar3">>).
-define(LANGUAGES , [<<"erlang">>]).
-define(ARGV      , [?NAME, <<"bsp">>]).
-define(BSP_DIR   , <<".bsp">>).
-define(FILENAME  , <<?NAME/binary, ".json">>).

%%==============================================================================
%% API
%%==============================================================================
-spec generate(string()) -> ok.
generate(BaseDir) ->
  Path = filepath(BaseDir),
  ok = filelib:ensure_dir(Path),
  Details = jsx:encode(details(), [space, indent]),
  ok = file:write_file(Path, Details).

-spec discover(string()) -> {ok, string(), [string()]}.
discover(BaseDir) ->
  [C|_] = filelib:wildcard(filename:join([BaseDir, ".bsp", "*.json"])),
  {ok, Content} = file:read_file(C),
  #{argv := [Cmd|Params]} = jsx:decode(Content, [return_maps, {labels, atom}]),
  E = os:find_executable(binary_to_list(Cmd)),
  Args = [binary_to_list(P) || P <- Params],
  {ok, E, Args}.

-spec exists(string()) -> boolean().
exists(BaseDir) ->
  filelib:is_regular(filepath(BaseDir)).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec details() -> details().
details() ->
  #{ name       => ?NAME
   , version    => version()
   , bspVersion => ?BSP_VSN
   , languages  => ?LANGUAGES
   , argv       => ?ARGV
   }.

-spec filepath(file:filename()) -> file:filename().
filepath(BaseDir) ->
  Dir = filename:join([BaseDir, ?BSP_DIR]),
  filename:join([Dir, ?FILENAME]).

-spec version() -> binary().
version() ->
  {ok, Vsn} = application:get_key(rebar3_bsp, vsn),
  list_to_binary(Vsn).
