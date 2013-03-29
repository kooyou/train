%%%------------------------------------
%%% @Module  : manager_client
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/29
%%% @Description: 管理所有连接的客户端
%%%------------------------------------

-module(manager_client).
-compile(export_all).
-export([start/0]).
%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================


start() ->
    io:format("manager_client starting...~n"),
    register(?MODULE,spawn_link(fun() -> manage_client([],chat_data) end)),
    {ok,whereis(?MODULE)}.

%管理所有客户端:
manage_client(ClientList,DataPid) ->
    receive
        {connected,Socket,UserId} ->
            %向在线表写入数据
            DataPid ! {get_user_name,UserId,self()},
            receive
                [{_,Name,_,_,_}] -> DataPid ! {add_online,Socket,UserId,Name};
                _Other -> void
            end,

            %%%%%%%%%% 测试警报%%%%%%%
            alarm_handler:set_alarm(tooMuchClient),
            %客户端登录成功，将客户端的Socket加入列表
            manage_client([Socket|ClientList],DataPid);
        {disconnected,Socket} ->
            %客户端下线，将客户端的Socket从列表删除
            NewList = lists:delete(Socket,ClientList),
            DataPid ! {del_user_info,Socket},
            DataPid ! {del_online,Socket},
            manage_client(NewList,DataPid);
        {send,Bin} ->
            %广播信息
            send_data(ClientList,Bin),
            manage_client(ClientList,DataPid);
        _Other ->
            manage_client(ClientList,DataPid)
    end.


%对客户端列表进行数据广播
send_data(SocketList,Bin) ->
    lists:foreach(fun(Socket) ->
                gen_tcp:send(Socket,Bin)
                  end,
                  SocketList).
