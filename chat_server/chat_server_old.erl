%%%-------------------------------------------------
%%%@Module :chat_server
%%%@Author :fengzhenlin
%%%@Email  :535826356@qq.com
%%%@Created :2013.3.22
%%%@Description : 聊天系统服务端
%%%-------------------------------------------------
-module(chat_server).
-compile(export_all).

-behaviour(gen_server).

%%gen_server callbacks
-export([inti/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
    code_change/3]).

%%=================================================
%%接口函数
%%=================================================
start() -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).
stop() -> gen_server:call(?MODULE,stop).


%%==================================================
%%回调函数
%%==================================================
init([]) -> start_parallel_server(),
    {ok,temp}.

start_parallel_server() ->
    {ok,Listen} = gen_tcp:listen(2345,[binary,{packet,4},
            {reuseaddr,true},
            {active,true}]),
    spawn


