-module(arrow).

-export([
    new_array/2,
    from_list/2,
    to_list/1
]).

-export_type([
    type/0,
    data_type/0
]).

-include("crates.hrl").

-type time_unit() :: s | ms | us | ns.
-type date_unit() :: d | ms.
-type interval_unit() :: year_month | day_time.
-type data_type() ::
      boolean
    | utf8
    | {int, 8 | 16 | 32 | 64}
    | {uint, 8 | 16 | 32 | 64}
    | {float, 32 | 64}
    | {date, date_unit()}
    | {time, time_unit()}
    | {timestamp, time_unit()}
    | {interval, interval_unit()}
    | {list, data_type()}
    | [{atom(), data_type()}]
    .

-opaque type() :: reference().

-on_load(init/0).

init() ->
    erlang:load_nif(?crate_arrow_nif, 0).

-spec new_array(Type :: data_type(), Length :: non_neg_integer()) -> {ok, type()} | {error, _}.
new_array(_Type, _Length) ->
    erlang:nif_error(nif_not_loaded).

-spec from_list(Type :: data_type(), list()) -> {ok, type()} | {error, _}.
from_list(_Type, _List) ->
    erlang:nif_error(nif_not_loaded).

-spec to_list(type()) -> list().
to_list(_Type) ->
    erlang:nif_error(nif_not_loaded).
