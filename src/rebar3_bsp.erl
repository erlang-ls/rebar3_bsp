-module(rebar3_bsp).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  rebar3_bsp_prv:init(State).
