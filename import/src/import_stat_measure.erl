-module(import_stat_measure).

-created('hejin 2014-7-20').

-include_lib("elog/include/elog.hrl").

-define(filename, "stat_measure_").

-define(metrics, [ekey, value]).

-export([start/2]).


start(JourDir, OutDir) ->
	start_app(),
	?ERROR("start_time: ~p,~p", [extbif:strftime(), JourDir]),
    {Hour, _, _} = time(),
    LastHour =
    if
    Hour == 0 ->
        23;
    true ->
        Hour - 1
    end,
	ets:new(records, [bag, named_table, protected]),
	
	read_journals(JourDir, LastHour),
    ?ERROR("end_of_readfile: ~p,~p", [JourDir, extbif:strftime()]),
	
    % EndTime = (extbif:timestamp() div 3600) * 3600,
    % BeginTime = EndTime - 3600,
	
    FileName = lists:concat([OutDir, "/", ?filename, extbif:zeropad(Hour), ".ctl"]),
	{ok, File} = file:open(FileName, [write]),
	% sqlldr:header(File, ImportTable, ?metrics),
	run(ets:first(records), File),
	
    ?ERROR("end_time: ~p", [extbif:strftime()]).
	
	
start_app() ->
    [application:start(App) || App <- [sasl, crypto, lager]].	

run('$end_of_table', File) ->
	file:close(File);
run(Key, File) ->
    Rows = ets:lookup(records, Key),
	Data = rollup_count(Key, Rows),
	DataLine = sqlldr:line(?metrics, Data),
	file:write(File, DataLine),
	run(ets:next(records, Key), File).

	
rollup_count(Key, Rows) ->
	Count = lists:foldl(fun({_, {_Time, Value}}, Acc) ->
			Acc + Value
	end, 0, Rows),
	[{value, Count}, {ekey, Key}].
	
read_journals(SrcDir, LastHour) ->
    Dir = lists:concat([SrcDir, "/", extbif:strfdate(date()), "/", extbif:zeropad(LastHour), "/"]),
    ?ERROR("begin to read journal: ~p", [Dir]),
    {ok, FileNames} = file:list_dir(Dir),
    lists:foreach(fun(Name) ->
        case lists:suffix(".journal", Name) of
            true ->
                Path = filename:join(Dir, Name),
                ?INFO("start to read file: ~p", [Path]),
                {ok, File} = file:open(Path, [read, binary, raw]),
                read(File);
            false ->
                ignore
        end
    end, FileNames).

read(File) ->
    case file:read_line(File) of
    {ok, Line} ->
        decode_line(Line),
        read(File);
    eof ->
        ?INFO("file close", []),
        file:close(File);
    {error, Reason} ->
        ?ERROR("read file error: ~p", [Reason])
    end.
	
	
decode_line(Line) ->
	case binary:split(Line, [<<"@">>, <<"|">>], [global]) of
		[_LogTime, Key, Time, Value] ->
			case check_stat_key(Key) of
				{true, KeyN} ->
					ets:insert(records, {KeyN, {binary_to_integer(Time), extbif:to_integer(Value)} } );
				false ->
					ignore
			end;
		_ ->
			ignore
	end.			
	
check_stat_key(Key)	->
	KeyL = binary:split(Key, <<":">>, [global]),
	case lists:last(KeyL) == <<"stat">> of
		true ->
			{true,  binary:part(Key, 0, size(Key) - 5))};
		false ->
			false
	end.		