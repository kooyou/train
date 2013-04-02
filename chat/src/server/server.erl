%%%------------------------------------
%%% @Module  : server
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: 聊天服务器
%%%------------------------------------

-module(server).
-behaviour(gen_server).
-compile(export_all).
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

-define(CHAT_DATA,chat_data).
-define(MANAGER_CLIENT,manager_client).


start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).
    

init([]) ->
    process_flag(trap_exit,true),
    io:format("~p starting~n",[?MODULE]),
    start(),
    {ok,whereis(?MODULE)}.

start() ->

    {ok,Listen} = gen_tcp:listen(2345,[binary,{packet,4},
            {reuseaddr,true},
            {active,true}]),

    %创建并行监听进程
    Pid = spawn_link(fun() -> par_connect(Listen,?MANAGER_CLIENT,?CHAT_DATA) end),
     gen_tcp:controlling_process(Listen,Pid),
    io:format("server running~n").


call_exception() -> gen_server:call(?MODULE,{9}).
%==============================================================

%回调函数
handle_call({Thing},_From,N) ->
    Thing/0,
    {reply,N,N}.
handle_cast(_Msg,N) -> {noreply,N}.
handle_info(_Info,N) -> {noreply,N}.
terminate(_Reason,_N) ->
    io:format("~p stopping~n",[?MODULE]),
    ok.
code_change(_OldVsn,N,_Extra) -> {ok,N}.
%===============================================================

%并行连接
par_connect(Listen,ManagerClientPid,DataPid) ->
   
    {ok,Socket} = gen_tcp:accept(Listen),
    
    %创建新进程进行新客户端的连接
%    spawn(fun() -> par_connect(Listen,ManagerClientPid,DataPid) end),

    %管理一个用户的连接
    Pid = spawn(fun()->connector:manage_one_connector(Socket,DataPid) end),
    gen_tcp:controlling_process(Socket,Pid),
    par_connect(Listen,ManagerClientPid,DataPid).

