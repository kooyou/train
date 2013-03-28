%%%------------------------------------
%%% @Module  : server
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: 聊天服务器
%%%------------------------------------

-module(server).
-behaviour(gen_server).
-export([start/0,start_link/0]).
%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).

%%========================================================================
%%数据
%%=========================================================================



-record(user,{
    id,             %用户ID
    name,           %用户名称
    passwd,         %用户登录密码
    login_times,    %登录次数
    chat_times,     %聊天次数
    last_login     %最后一次登录时间
}).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
    process_flag(trap_exit,true),
    io:format("~p starting~n",[?MODULE]),
    start(),
    {ok,0}.

start() ->
    %初始化数据
    Spid = self(),
    DataPid = spawn(fun() -> chat_data:data_init(Spid) end),
    receive
        {datainit,ok} -> void;
        _Other -> start()
    end,

    {ok,Listen} = gen_tcp:listen(2345,[binary,{packet,4},
            {reuseaddr,true},
            {active,true}]),

    ManagerClientPid = spawn(fun() -> manage_client([],DataPid) end),
    spawn(fun() -> par_connect(Listen,ManagerClientPid,DataPid) end),
    io:format("server running~n").

%==============================================================
%回调函数
handle_call(_Msg,_From,N) ->
    {reply,N,N}.
handle_cast(_Msg,N) -> {noreply,N}.
handle_info(_Info,N) -> {noreply,N}.
terminate(_Reason,_N) ->
    io:format("~p stopping~",[?MODULE]),
    ok.
code_change(_OldVsn,N,_Extra) -> {ok,N}.
%===============================================================

%并行连接
par_connect(Listen,ManagerClientPid,DataPid) ->
    {ok,Socket} = gen_tcp:accept(Listen),
    
    %创建新进程进行新客户端的连接
    spawn(fun() -> par_connect(Listen,ManagerClientPid,DataPid) end),

    %管理一个用户的连接
    connector:manage_one_connector(Socket,ManagerClientPid,DataPid).



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
