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


%�򵥵���1-N�������ݣ�һ������N��
insert(1,TableId) ->
    ets:insert(TableId,{1,'Tom',1,3.0});
insert(N,TableId) ->
    ets:insert(TableId,{2+N-1,'Tom',1,3.0}),
    insert(N-1,TableId).

%�򵥵Ĵ�1-Key����ѯ���ݣ�һ����ѯKey��
lookup(1,TableId) ->
    ets:lookup(TableId,1);
lookup(Key,TableId) ->
    ets:lookup(TableId,Key),
    lookup(Key-1,TableId).

%���Բ�����������ʱ��
test_insert(InsertTimes,Mode) ->
    TableId = ets:new(test,[Mode]),
    {M,_} = timer:tc(?MODULE,insert,[InsertTimes,TableId]),
    io:format("Module ~p insert = ~p micro secondes~n",[Mode,M]),
    {TableId}.  %����est���ID��������в�ѯ����ʱ��Ҫ�õ��ñ��ID

%���Բ�ѯ��������ʱ��
test_lookup(Number,TableId) ->
    {M,_} = timer:tc(?MODULE,lookup,[Number,TableId]),
    io:format("lookup = ~p micro secondes~n",[M]).
