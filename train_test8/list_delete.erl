%%%-------------------------------------------------
%%%@Module :list_delete
%%%@Author :fengzhenlin
%%%@Email  :535826356@qq.com
%%%@Created :2013.3.20
%%%@Description : 实现erlang内置的"--"列表删除操作符
%%%--------------------------------------------------

-module(list_delete).
%-compile(export_all).
-export([del_list/2]).

%标识列表元素的顺序
%结果[b,a,c] -> [{b,1},{a,2},{c,3}]
order_list([Head|L],N) -> [{Head,N}] ++ order_list(L,N+1);
order_list([],N) -> [].

%实现列表的"--"操作
del_list([{Key,Order}|SortList],[SecHead|SortDelList]) ->
    if
        Key =/= SecHead ->  %比较的元素不相等，将元素保存下来
            if
                Key > SecHead ->
                    %将删除列表往后推
                    del_list([{Key,Order}|SortList],SortDelList);
                true ->
                    [{Key,Order}] ++ del_list(SortList,[SecHead|SortDelList])
            end;
        Key =:= SecHead ->  %比较的元素相等，删除元素
            del_list(SortList,SortDelList)
    end;

del_list([],L) -> [];
del_list(L,[]) -> L.


%根据有序标识，将列表还原成一般列表
%结果 [{a,2},{b,1},{c,3}] -> [b,a,c]
restore_list(L) ->
    OrderList = lists:keysort(2,L),
    ResultList = get_key_list(OrderList),
    {ResultList}.

get_key_list([{Key,Order}|L]) ->
    [Key] ++ get_key_list(L);

get_key_list([]) -> [].


%for 循环
for(Max,Max,F) -> [F(Max)];
for(I,Max,F) -> [F(Max) | for(I+1,Max,F)].

%随机产生元素个数为N的范围在1-N的列表
random_list(N) -> 
    for(1,N,fun(N) -> random:uniform(N) end).

%实现 Originallist -- DelList
list_delete(OriginalList,DelList) -> 
    OrderList = order_list(OriginalList,0),
    SortList = lists:keysort(1,OrderList),
    SortDelList = lists:sort(DelList),
    DeledList = del_list(SortList,SortDelList),
    ResultList = restore_list(DeledList),
    io:format("~p~n",[ResultList]),
    {ResultList}.

%erlang内置的列表"--"操作
origin_list_del(OriginalList,DelList) ->
    io:format("~p~n",[OriginalList -- DelList]).

%测试自己写的"--"操作和erlang内置的"--"操作作比较
%参数N,M为目标列表和删除列表的长度。列表数据都是自动生成的。
list_del(N,M) ->
    random:seed(now()),
    OriginalList = random_list(N),
    DelList = random_list(M),
    {Time,_} = timer:tc(?MODULE,list_delete,[OriginalList,DelList]),
    {Time1,_} = timer:tc(?MODULE,origin_list_del,[OriginalList,DelList]),
    io:format("my algorithm uses time ~p micro seconds~n",[Time]),
    io:format("erlang lists uses time ~p micro seconds~n",[Time1]).
    
