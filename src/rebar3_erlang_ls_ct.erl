-module(rebar3_erlang_ls_ct).

-export([ run_test/2 ]).

-spec run_test(atom(), atom()) -> ok | {error, any()}.
run_test(Suite, Case) ->
  register(erlang_ls, self()),
  rebar3:run([ "ct"
             , io_lib:format("--suite=~s", [Suite])
             , io_lib:format("--case=~s", [Case])
             ]),
  receive {result, Result} ->
      Result
  end.
