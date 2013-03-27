-module(chat_supervisor).
-behaviour(supervisor).

-export([start/0,init/1]).

start() ->
       supervisor:start_link({local,?MODULE},?MODULE,_Arg=[]).

init([]) ->
    %%安装服务器警报管理
    gen_event:swap_handler(alarm_handler,
        {alarm_handler,swap},
        {server_alarm,xyz}),

    {ok,{{one_for_one,3,10},
            [{tag1,
             {server,start_link,[]},
              permanent,
              10000,
              worker,
              [server]}
    ]}}.
