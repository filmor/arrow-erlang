-module(arrow).

-export([
    test/0
]).

-on_load(init/0).

init() ->
    {ok, Lib} = find_crate:find_library(arrow, "arrow_nif"),
    erlang:load_nif(Lib, 0).


test() -> error.
