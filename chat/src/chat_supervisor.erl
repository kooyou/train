-module(chat_supervisor).
-behaviour(supervisor).

-export([start/0,init/1]).

start() ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init([]) ->
    %%安装服务器警报管理
    gen_event:swap_handler(alarm_handler,
        {alarm_handler,swap},
        {server_alarm,xyz}),

    {ok,{
         {one_for_one,3,3600},
        [
        %数据库管理进程
        {spec_chat_data,
            {chat_data,start,[]},
            permanent,
            brutal_kill,
            worker,
            [chat_data]
        },
        %连接用户群管理进程
        {spec_manage_client,
             {manager_client,start,[]},
             permanent,
             brutal_kill,
             worker,
             [manager_client]
        },
        %服务端监控进程
       {spec_chat_server,
            {server,start_link,[]},
            permanent,
            brutal_kill,
            worker,
            [server]}
    ]}}.
