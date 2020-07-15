-module(rebar3_bsp_stdio).

-export([ start_link/0 ]).
-export([ init/0
        , loop/1
        ]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  {ok, proc_lib:spawn_link(?MODULE, init, [])}.

-spec init() -> no_return().
init() ->
  ok = io:setopts(standard_io, [binary]),
  ?MODULE:loop([]).

-spec loop([binary()]) -> no_return().
loop(Lines) ->
  case io:get_line(standard_io, "") of
    <<"\n">> ->
      Headers = parse_headers(Lines),
      BinLength = proplists:get_value(<<"content-length">>, Headers),
      Length = binary_to_integer(BinLength),
      {ok, Payload} = file:read(standard_io, Length),
      #{ <<"method">> := Method
       , <<"params">> := Params
       } = jsx:decode(Payload, [return_maps]),
      %% TODO: Add protocol and jsx as deps
      Result = rebar3_bsp_agent:handle_request(Method, Params),
      file:write_file("/tmp/result", [term_to_binary(Result)]),
      %% TODO: Hard-coded request id
      %% TODO: Distinguish between notifications and responses
      Response = els_protocol:response(1, Result),
      file:write_file("/tmp/response", [term_to_binary(Response)]),
      io:format(standard_io, "~s", [Response]),
      ?MODULE:loop([]);
    eof ->
      todo;
    Line ->
      ?MODULE:loop([Line | Lines])
  end.

-spec parse_headers([binary()]) -> [{binary(), binary()}].
parse_headers(Lines) ->
  [parse_header(Line) || Line <- Lines].

-spec parse_header(binary()) -> {binary(), binary()}.
parse_header(Line) ->
  [Name, Value] = binary:split(Line, <<":">>),
  {string:trim(string:lowercase(Name)), string:trim(Value)}.
