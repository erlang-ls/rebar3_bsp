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
      Request = jsx:decode(Payload, [return_maps]),
      #{ <<"id">> := Id
       , <<"method">> := Method
       , <<"params">> := Params } = Request,
      RequestType = request_type(Request),
      case RequestType of
        notification ->
          ok = rebar3_bsp_agent:handle_notification(Method, Params);
        request ->
          Result = rebar3_bsp_agent:handle_request(Method, Params),
          Response = rebar3_bsp_protocol:response(Id, Result),
          io:format(standard_io, "~s", [Response])
      end,
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

-spec request_type(map()) -> notification | request.
request_type(#{<<"id">> := _Id}) ->
  request;
request_type(_) ->
  notification.
