-module(arrow).

-export([
    new_array/2
]).

-export_type([
    type/0,
    data_type/0
]).

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
    {ok, Lib} = find_crate:find_library(arrow, "arrow_nif"),
    erlang:load_nif(Lib, 0).

-spec new_array(Type :: data_type(), Length :: non_neg_integer()) -> {ok, type()} | {error, _}.
new_array(_Type, _Length) ->
    erlang:nif_error(nif_not_loaded).