%%%-------------------------------------------------
%%%@Module :list_delete
%%%@Author :fengzhenlin
%%%@Email  :535826356@qq.com
%%%@Created :2013.3.20
%%%@Description : ʵ��erlang���õ�"--"�б�ɾ��������
%%%--------------------------------------------------

-module(list_delete).
%-compile(export_all).
-export([del_list/2]).

%��ʶ�б�Ԫ�ص�˳��
%���[b,a,c] -> [{b,1},{a,2},{c,3}]
order_list([Head|L],N) -> [{Head,N}] ++ order_list(L,N+1);
order_list([],N) -> [].

%ʵ���б��"--"����
del_list([{Key,Order}|SortList],[SecHead|SortDelList]) ->
    if
        Key =/= SecHead ->  %�Ƚϵ�Ԫ�ز���ȣ���Ԫ�ر�������
            if
                Key > SecHead ->
                    %��ɾ���б�������
                    del_list([{Key,Order}|SortList],SortDelList);
                true ->
                    [{Key,Order}] ++ del_list(SortList,[SecHead|SortDelList])
            end;
        Key =:= SecHead ->  %�Ƚϵ�Ԫ����ȣ�ɾ��Ԫ��
            del_list(SortList,SortDelList)
    end;

del_list([],L) -> [];
del_list(L,[]) -> L.


%���������ʶ�����б�ԭ��һ���б�
%��� [{a,2},{b,1},{c,3}] -> [b,a,c]
restore_list(L) ->
    OrderList = lists:keysort(2,L),
    ResultList = get_key_list(OrderList),
    {ResultList}.

get_key_list([{Key,Order}|L]) ->
    [Key] ++ get_key_list(L);

get_key_list([]) -> [].


%for ѭ��
for(Max,Max,F) -> [F(Max)];
for(I,Max,F) -> [F(Max) | for(I+1,Max,F)].

%�������Ԫ�ظ���ΪN�ķ�Χ��1-N���б�
random_list(N) -> 
    for(1,N,fun(N) -> random:uniform(N) end).

%ʵ�� Originallist -- DelList
list_delete(OriginalList,DelList) -> 
    OrderList = order_list(OriginalList,0),
    SortList = lists:keysort(1,OrderList),
    SortDelList = lists:sort(DelList),
    DeledList = del_list(SortList,SortDelList),
    ResultList = restore_list(DeledList),
    io:format("~p~n",[ResultList]),
    {ResultList}.

%erlang���õ��б�"--"����
origin_list_del(OriginalList,DelList) ->
    io:format("~p~n",[OriginalList -- DelList]).

%�����Լ�д��"--"������erlang���õ�"--"�������Ƚ�
%����N,MΪĿ���б��ɾ���б�ĳ��ȡ��б����ݶ����Զ����ɵġ�
list_del(N,M) ->
    random:seed(now()),
    OriginalList = random_list(N),
    DelList = random_list(M),
    {Time,_} = timer:tc(?MODULE,list_delete,[OriginalList,DelList]),
    {Time1,_} = timer:tc(?MODULE,origin_list_del,[OriginalList,DelList]),
    io:format("my algorithm uses time ~p micro seconds~n",[Time]),
    io:format("erlang lists uses time ~p micro seconds~n",[Time1]).
    
