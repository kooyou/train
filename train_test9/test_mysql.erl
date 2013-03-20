%%%------------------------------
%%%@Module :test_mysql
%%%@Author :fengzhenlin
%%%@Email  :535826356@qq.com
%%%@Created :2013.3.19
%%%@Description :testing mysql
%%%------------------------------
-module(test_mysql).
-export([test_insert/1,test_select/1,insert/1,select/1]).
%-compile(export_all).


%递归向mysql表插入数据。
%这里为了测试方便，表定为student，插入的数据也是一样的
%参数N代表插入N条数据
insert(1) -> mysql:fetch(p1,<<"insert into student values(1,'Tom',1,3.0)">>);

insert(N) -> 
    TempStr = io_lib:format(
                "insert into student values(~p,'Tom',1,4.0)",[N]),
    SqlStr = lists:flatten(TempStr),
    mysql:fetch(p1,SqlStr),
    insert(N-1).

for(Max,Max,F) -> [F(Max)];
for(I,Max,F) -> [F(Max) | for(I+1,Max,F)].

%直接向表student查询条数据
%参数N表示查询N条数据,查询的条件随机产生
select(N) ->
    for(1,N,fun(N) -> 
                RandomN = random:uniform(N),
                SelectStr = io_lib:format(
                    "select * from student where id = ~p",[RandomN]),
                SqlStr = lists:flatten(SelectStr),
                %io:format("~p ~n",[SqlStr]),
                io:format("~p ~n",[mysql:fetch(p1,SqlStr)]) end).


%测试插入效率
test_insert(InsertTimes) ->
    %打开数据库test
    mysql:start_link(p1,"localhost","root","123","test"),
    %insert(InsertTimes).
    {M,_} = timer:tc(?MODULE,insert,[InsertTimes]),
    io:format(" insert = ~p micro seconds~n",[M]).


%测试查询效率
test_select(SelectNum) -> 
    %打开数据库test
    mysql:start_link(p1,"localhost","root","123","test"),
    {M,_} = timer:tc(?MODULE,select,[SelectNum]),
    io:format(" select = ~p micro seconds~n",[M]).
