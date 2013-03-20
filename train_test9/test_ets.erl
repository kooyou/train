%%%------------------------------
%%%@Module :test_ets
%%%@Author :fengzhenlin
%%%@Email  :535826356@qq.com
%%%@Created :2013.3.19
%%%@Description :testing ets
%%%------------------------------

-module(test_ets).
-export([test_insert/2,test_lookup/2,insert/2,lookup/2]).
%-compile(export_all).


%简单的以1-N插入数据，一共插入N次
insert(1,TableId) ->
    ets:insert(TableId,{1,'Tom',1,3.0});
insert(N,TableId) ->
    ets:insert(TableId,{2+N-1,'Tom',1,3.0}),
    insert(N-1,TableId).

%简单的从1-Key来查询数据，一共查询Key次
lookup(1,TableId) ->
    ets:lookup(TableId,1);
lookup(Key,TableId) ->
    ets:lookup(TableId,Key),
    lookup(Key-1,TableId).

%测试插入数据所需时间
test_insert(InsertTimes,Mode) ->
    TableId = ets:new(test,[Mode]),
    {M,_} = timer:tc(?MODULE,insert,[InsertTimes,TableId]),
    io:format("Module ~p insert = ~p micro secondes~n",[Mode,M]),
    {TableId}.  %返回est表的ID，后面进行查询测试时需要用到该表的ID

%测试查询数据所需时间
test_lookup(Number,TableId) ->
    {M,_} = timer:tc(?MODULE,lookup,[Number,TableId]),
    io:format("lookup = ~p micro secondes~n",[M]).
