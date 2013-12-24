-module(mon_unix).

-include_lib("elog/include/elog.hrl").

-export([run/1]).

run(Args) ->
	Ts = extbif:timestamp(),
    SID = proplists:get_value(sid, Args),
    %%host info
    HostInfo = [{ip_addrs, inet_info()}, 
                {cpu_info, cpu_info()}, 
                {mem_info, mem_info()}, 
                {swap_info, swap_info()}, 
                {disk_info, disk_info()}],
    %datalogs
    Datalogs = [cpu_datalog(SID, Ts), task_datalog(SID, Ts), 
        mem_datalog(SID, Ts) | disk_datalogs(SID, Ts)],

    {ok, HostInfo, Datalogs}.

cpu_datalog(SID, Ts) ->
    ?INFO("cpu datalog", []),
	{datalog, <<"server.localcpu">>, SID, Ts, [
        {cpu1min, cpu_sup:avg1() / 256}, 
        {cpu5min, cpu_sup:avg5() / 256}, 
        {cpu15min, cpu_sup:avg15() / 256}]}. 

task_datalog(SID, Ts) ->
    ?INFO("task datalog", []),
	{datalog, <<"server.localtask">>, SID, Ts, [
		{taskTotal, cpu_sup:nprocs()}]}.

mem_datalog(SID, Ts) ->
    ?INFO("mem datalog", []),
    Dataset = memsup:get_system_memory_data(),
    MemTotal = proplists:get_value(total_memory, Dataset),
    MemFree = proplists:get_value(free_memory, Dataset),
    MemUsed = MemTotal - MemFree,
    SwapTotal = proplists:get_value(total_swap, Dataset, 0),
    SwapFree = proplists:get_value(free_swap, Dataset, 0),
    SwapUsed = SwapTotal - SwapFree,
    {datalog, <<"server.localmem">>, SID, Ts, [
        {memTotal, MemTotal}, {memUsed, MemUsed}, {memFree, MemFree},
        {swapTotal, SwapTotal}, {swapUsed, SwapUsed}, {swapFree, SwapFree}]}.

disk_datalogs(SID, Ts) ->
    ?INFO("disk datalogs", []),
    Disks = disksup:get_disk_data(),
    lists:map(fun({Dev, DiskTotal, Usage}) -> 
        DiskDn = list_to_binary(["disk=", Dev, ",", SID]),
        DiskUsed = (DiskTotal * Usage) div 100,
        DiskAvail = DiskTotal - DiskUsed,
        {datalog, <<"server.localdisk">>, DiskDn, Ts, [
            {diskTotal, DiskTotal}, {diskUsed, DiskUsed}, {diskFree, DiskAvail}]}
    end, Disks).

cpu_info() ->
    Load1 = ftos(cpu_sup:avg1() / 256),
    Load5 = ftos(cpu_sup:avg5() / 256),
    Load15 = ftos(cpu_sup:avg15() / 256),
    lists:concat(["load1=", Load1, ", load5=", Load5, ", load15=", Load15]).

mem_info() ->
    Dataset = memsup:get_system_memory_data(),
    MemTotal = proplists:get_value(total_memory, Dataset),
    MemFree = proplists:get_value(free_memory, Dataset),
    MemUsed = MemTotal - MemFree,
    lists:concat(["total=", MemTotal, ", used=", MemUsed, ", free=", MemFree]).
    
swap_info() ->
    Dataset = memsup:get_system_memory_data(),
    SwapTotal = proplists:get_value(total_swap, Dataset, 0),
    SwapFree = proplists:get_value(free_swap, Dataset, 0),
    SwapUsed = SwapTotal - SwapFree,
    lists:concat(["total=", SwapTotal, ", used=", SwapUsed, ", free=", SwapFree]).
    
disk_info() ->
    Disks = disksup:get_disk_data(),
    Lines = lists:map(fun({Dev, Total, Usage}) -> 
        Used = (Total * Usage) div 100,
        Avail = Total - Used,
        lists:concat(["dev=", Dev, ", total=", Total, 
            ", avail=", Avail, ", used=", Used, ", usage=", Usage, "%"])
    end, Disks),
    string:join(Lines, "\n").

inet_info() ->
    {ok, IfNames} = inet:getiflist(),
    IfList = [ begin {ok, [{addr, Addr}|_]} = inet:ifget(IfName, [addr]), {IfName, inet_parse:ntoa(Addr)} end || IfName <- IfNames],
    IfList1 = [E || {_Name, Addr} = E <- IfList, Addr =/= "127.0.0.1"],
    string:join([Name ++ ": " ++ Addr || {Name, Addr} <- IfList1], ",").

ftos(F) ->
    [S] = io_lib:format("~.2f", [F]),
    S.