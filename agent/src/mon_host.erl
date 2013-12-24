-module(mon_host).

-include_lib("elog/include/elog.hrl").

-export([run/1]).

run(Args) ->
	Timestamp = timestamp(),
    SID = proplists:get_value(sid, Args),
	
	TopInfo = os:cmd("top -b -n 1"),
	[Summary, TaskInfo, CpuInfo, MemInfo, SwapInfo | _] = string:tokens(TopInfo, "\n"),
    %%cpu info
	[Cpu15Min, Cpu5Min, Cpu1Min | _] = lists:reverse(string:tokens(Summary, ", ")),
	CpuDatalog = {datalog, <<"server.localcpu">>, SID, Timestamp, [
		{cpu1min, list_to_float(Cpu1Min)}, {cpu5min, list_to_float(Cpu5Min)}, {cpu15min, list_to_float(Cpu15Min)}]},

    %%task info
	[_, TaskStats | _] = string:tokens(TaskInfo, ":"),
	[TaskTotal | _] = string:tokens(TaskStats, " "),
	TaskDatalog = {datalog, <<"server.localtask">>, SID, Timestamp, [
		{taskTotal, list_to_integer(TaskTotal)}]},

    %%mem info
	[_, MemUsage | _] = string:tokens(MemInfo, ":"),
	[MemTotal,"total", MemUsed, "used", MemFree, "free" | _] = string:tokens(MemUsage, ", "),
	[MemTotalNum, MemUsedNum, MemFreeNum] = lists:map(fun(Mem) ->
			list_to_integer(string:substr(Mem, 1, length(Mem) -1))  * 1024
		end, [MemTotal, MemUsed, MemFree]),
	[_, SwapUsage | _] = string:tokens(SwapInfo, ":"),
	[SwapTotal,"total", SwapUsed, "used", SwapFree, "free" | _] = string:tokens(SwapUsage, ", "),
	[SwapTotalNum, SwapUsedNum, SwapFreeNum] = lists:map(fun(Swap) ->
			list_to_integer(string:substr(Swap, 1, length(Swap) -1)) * 1024
		end, [SwapTotal, SwapUsed, SwapFree]),
	MemDatalog = {datalog, <<"server.localmem">>, SID, Timestamp, [
		{memTotal, MemTotalNum}, {memUsed, MemUsedNum}, {memFree, MemFreeNum},
		{swapTotal, SwapTotalNum}, {swapUsed, SwapUsedNum}, {swapFree, SwapFreeNum}]},

    %%disk info
    {DiskInfo, DiskDatalogs} = disk_info(SID, Timestamp),

    %%host info
    NetInfo = string:join([Name ++ ": " ++ Addr|| {Name, Addr} <- inet_info()], ","),
    HostInfo = [{ip_addrs, NetInfo}, {cpu_info, CpuInfo}, {mem_info, MemInfo}, {swap_info, SwapInfo}, {disk_info, DiskInfo}],

    {ok, HostInfo, [CpuDatalog, TaskDatalog, MemDatalog | DiskDatalogs]}.

inet_info() ->
    %{ok,["lo","eth0","eth1"]}
    {ok, IfNames} = inet:getiflist(),
    IfList = [ begin {ok, [{addr, Addr}|_]} = inet:ifget(IfName, [addr]), {IfName, inet_parse:ntoa(Addr)} end || IfName <- IfNames],
    [E || {_Name, Addr} = E <- IfList, Addr =/= "127.0.0.1"].

disk_info(SID, Timestamp) ->
	DfInfo = os:cmd("df -t ext3"),
	[_|Disks] = string:tokens(DfInfo, "\n"),
    Disks1 = formalize(Disks, []),
    DiskInfo = string:join(Disks1, "\n"),
    Datalogs = lists:map(fun(Disk) ->
        [Dev, Total, Used, Available, _Percent, Fs] = string:tokens(Disk, " "),
        DiskDn = lists:concat(["disk=", string:join(string:tokens(Dev, "/"), "%"), ",", SID]),
        [DiskTotal, DiskUsed, DiskAvail] = [list_to_integer(Size) * 1000 || Size <- [Total, Used, Available]],
        {datalog, <<"server.localdisk">>, DiskDn, Timestamp, [{filesystem, Fs},
            {diskTotal, DiskTotal}, {diskUsed, DiskUsed}, {diskFree, DiskAvail}]}
    end, Disks1),
    {DiskInfo, Datalogs}.

formalize([], Acc) ->
    Acc;
formalize([DiskInfo|T], Acc) ->
    case length(string:tokens(DiskInfo, " ")) of
    1 ->
        [MoreInfo|T1] = T,
        formalize(T1, [DiskInfo ++ " " ++ MoreInfo | Acc]);
    _ ->
        formalize(T, [DiskInfo|Acc])
    end.

timestamp() ->
	{MegaSecs, Secs, _MicroSecs} = erlang:now(),
	MegaSecs * 1000000 + Secs.