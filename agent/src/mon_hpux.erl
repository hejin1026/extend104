-module(mon_hpux).

-include_lib("elog/include/elog.hrl").

-export([run/1]).

run(Args) ->
	Ts = extbif:timestamp(),
    {value, Dn} = dataset:get_value(dn, Args),
    TopOutput = os:cmd("top -d 1 -n 1"),
    ?INFO("~p", [TopOutput]),
    {CpuInfo, CpuDatalog, TaskDatalog} = cputask(Dn, Ts, TopOutput),
    MemOutput = os:cmd("swapinfo -tam"),
    {MemInfo, SwapInfo, MemDatalog} = swapmem(Dn, Ts, MemOutput),
    DiskOutput = os:cmd("/usr/bin/df -k"),
    {DiskInfo, DiskDatalogs} = disk(Dn, Ts, DiskOutput),
    %%host info
    HostInfo = [{ip_addrs, inet_info()}, 
                {cpu_info, CpuInfo}, 
                {mem_info, MemInfo}, 
                {swap_info, SwapInfo}, 
                {disk_info, DiskInfo}],
    %datalogs
    Datalogs = [CpuDatalog, MemDatalog, TaskDatalog | DiskDatalogs],
    {ok, HostInfo, Datalogs}.

cputask(Dn, Ts, Output) ->
    Lines = string:tokens(Output, "\r\n"),
    {Load1, Load5, Load15} = 
    case re:run(lists:nth(2, Lines), "Load averages:\\s+(\\d+.\\d+),\\s+(\\d+.\\d+),\\s+(\\d+.\\d+)", [{capture, [1,2,3], list}]) of
    {match, [SLoad1, SLoad5, SLoad15]} ->
        {SLoad1, SLoad5, SLoad15};
    nomatch ->
        ?WARNING("cpu nomatch", []),
        {"0.0", "0.0", "0.0"}
    end,
    CpuInfo = lists:concat(["load1=", Load1, ", load5=", Load5, ", load15=", Load15]),
    CpuDatalog = {datalog, <<"opengoss.localcpu">>, Dn, Ts, [
        {cpu1min, list_to_float(Load1)}, 
        {cpu5min, list_to_float(Load5)}, 
        {cpu15min, list_to_float(Load15)}]}, 
    TaskTotal =
    case re:run(lists:nth(3, Lines), "(\\d+)\\s+processes", [{capture, [1], list}]) of
    {match, [Total]} ->
        Total;
    nomatch ->
        ?WARNING("task nomatch", []),
        0
    end,
	TaskDatalog = {datalog, <<"opengoss.localtask">>, Dn, Ts, [
		{taskTotal, TaskTotal}]},
    {CpuInfo, CpuDatalog, TaskDatalog}.

swapmem(Dn, Ts, Output) ->
    Lines = string:tokens(Output, "\r\n"),
    {MemTotal, MemUsed, MemFree} = 
    case re:run(lists:nth(5, Lines), "memory\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+", 
        [{capture, [1,2,3], list}]) of
    {match, [Avail,Used,Free]} ->
        list_to_tuple([list_to_integer(L) * 1024 * 1024 || L <- [Avail, Used, Free]]);
    nomatch ->
        ?WARNING("mem nomatch: ~p", [lists:nth(3, Lines)]),
        {0,0,0}
    end,
    MemUsage = 
    case MemTotal == 0 of
    true -> 0;
    false -> (MemUsed * 100) div MemTotal
    end,
    MemInfo = lists:concat(["total=", MemTotal, ", used=", MemUsed, 
        ", free=", MemFree, ", usage=", MemUsage, "%"]),

    {SwapTotal, SwapUsed, SwapFree} = 
    case re:run(lists:nth(3, Lines), "dev\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+", [{capture, [1,2,3], list}]) of
    {match, [Avail1,Used1,Free1]} ->
        list_to_tuple([list_to_integer(L) * 1024 * 1024 || L <- [Avail1, Used1, Free1]]);
    nomatch ->
        ?WARNING("swap nomatch: ~p", [lists:nth(5, Lines)]),
        {0,0,0}
    end,
    SwapUsage = 
    case SwapTotal == 0 of
    true -> 0;
    false -> (SwapUsed * 100) div SwapTotal
    end,
    SwapInfo = lists:concat(["total=", SwapTotal, ", used=", SwapUsed, 
        ", free=", SwapFree, ", usage=", SwapUsage, "%"]),

    MemDatalog = {datalog, <<"opengoss.localmem">>, Dn, Ts, [
        {memTotal, MemTotal}, {memUsed, MemUsed}, {memFree, MemFree}, {memUsage, MemUsage},
        {swapTotal, SwapTotal}, {swapUsed, SwapUsed}, {swapFree, SwapFree}, {swapUsage, SwapUsage}]},
    {MemInfo, SwapInfo, MemDatalog}.

disk(Dn, Ts, Output) ->
    Lines = string:tokens(Output, "\r\n"),
    parse(Dn, Ts, Lines).

parse(Dn, Ts, Lines) ->
    parse(Dn, Ts, Lines, [], []).

parse(_Dn, _Ts, Lines, InfoAcc, DatalogAcc) when length(Lines) < 4 ->
    {string:join(lists:reverse(InfoAcc), "\n"), DatalogAcc};

parse(Dn, Ts, [L1,L2,L3,L4|Lines], InfoAcc, DatalogAcc) ->
    Tokens = string:tokens(L1, " "),
    Dev = lists:nth(1, Tokens),
    Total = list_to_integer(lists:nth(5, Tokens)),
    Free = list_to_integer(lists:nth(1, string:tokens(L2, " "))),
    Used = list_to_integer(lists:nth(1, string:tokens(L3, " "))),
    Usage = list_to_integer(lists:nth(1, string:tokens(L4, " "))),
    Info = lists:concat(["dev=", Dev, ", total=", Total, 
        "(KB), avail=", Free, "(KB), used=", Used, 
        "(KB), usage=", Usage, "%"]),
    DiskDn = lists:concat(["disk=", Dev, ",", Dn]),
    Datalog = {datalog, <<"opengoss.localdisk">>, DiskDn, Ts, [
            {diskTotal, Total}, {diskUsed, Used}, 
            {diskFree, Free}, {diskUsage, Usage}]},
    parse(Dn, Ts, Lines, [Info|InfoAcc], [Datalog|DatalogAcc]).

inet_info() ->
    {ok, IfNames} = inet:getiflist(),
    IfList = [ begin {ok, [{addr, Addr}|_]} = inet:ifget(IfName, [addr]), {IfName, inet_parse:ntoa(Addr)} end || IfName <- IfNames],
    IfList1 = [E || {_Name, Addr} = E <- IfList, Addr =/= "127.0.0.1"],
    string:join([Name ++ ": " ++ Addr || {Name, Addr} <- IfList1], ",").

