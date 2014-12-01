-module(sqlldr).

-include_lib("elog/include/elog.hrl").

-export([header/3, line/2]).

%--------------------------------------------------
% write sqlldr file
%--------------------------------------------------
header(File, Table, Columns) ->
	% Sql = "create table if not exists stat_measure_temp(ekey varchar(128), time datetime, value varchar(30), index ekey_index(ekey));",
    file:write(File, "INFILE INTO "++ Table ++ "(" ++ columns(Columns) ++ ")" ++ " values\n").

columns(List) ->
    HdStr = string:join(lists:reverse(List), ","),
    lists:concat(["(", HdStr, ")\n"]).


line(Metrics, Item) ->
    MetricValues = [str(proplists:get_value(Name, Item, "")) ||  Name <- Metrics],
    list_to_binary([string:join(MetricValues, "|"), <<"\n">>]).


str(V) when is_integer(V) ->
    integer_to_list(V);

str(V) when is_float(V) ->
    [S] = io_lib:format("~.6f", [V]),
    S;

str(V) when is_binary(V) ->
    binary_to_list(V);

str(V) when is_list(V) -> %list
    V;

str(V) ->
    ?ERROR("val with unknown type: ~p", [V]),
    "".
