%%%------------------------------------
%%% @Module  : server
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: 聊天服务器
%%%------------------------------------

-module(server).
-compile(export_all).

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

%%========================================================================
%%数据
%%=========================================================================
-ifdef(debug).
-define(TRACE(X),io:format("TRACE ~p:~p ~p~n",[?MODULE,?LINE,X])).
-else.
-define(TRACE(X),void).
-endif.

-define(LOGIN_COMMAND_ID,10001).
-define(SUCCEED,1).
-define(FALSE,1).

start() ->
    %初始化数据
    TabId = init(),

    {ok,Listen} = gen_tcp:listen(2345,[binary,{packet,4},
            {reuseaddr,true},
            {active,true}]),
    ManagerClientPid = spawn(fun() -> manage_client([]) end),
    spawn(fun() -> par_connect(Listen,ManagerClientPid,TabId) end).

%初始化数据表，并添加虚拟数据
init() ->
    %user表:user_id,psw
    UserTab = ets:new(user,[set]),

    ets:insert(UserTab,{100,"0"}),
    ets:insert(UserTab,{101,"1"}),
    ets:insert(UserTab,{102,"2"}),
    ets:insert(UserTab,{103,"3"}),

    %userInfo表:user_id,name,login_times,chat_times,last_login
    UserInfo = ets:new(userInfo,[set]),
    ets:insert(UserInfo,{100,"joe",0,0,0}),
    ets:insert(UserInfo,{101,"jane",0,0,0}),
    ets:insert(UserInfo,{102,"tom",0,0,0}),
    ets:insert(UserInfo,{103,"zhang",0,0,0}),

    %online表:name,socket
    Online = ets:new(online,[set]),
    
    TabId={UserTab,UserInfo,Online}.

%并行连接
par_connect(Listen,ManagerClientPid,TabId) ->
    {ok,Socket} = gen_tcp:accept(Listen),
    
    %创建新进程进行新客户端的连接
    spawn(fun() -> par_connect(Listen,ManagerClientPid,TabId) end),

    %验证登录的用户名和密码
    case auth_login(TabId) of
        {login,ok} -> 
            ManagerClientPid ! {connected,Socket},
            loop(Socket,ManagerClientPid);
        {login,error} ->
            void
    end.

%验证登录
auth_login(TabId) ->
    receive
        {tcp,Socket,Bin} ->
            io:format("meyoujinlai"),
            %登录协议解包
            <<Message_Len:16,Command_ID:16,
            UserId:32,StrLen:16,Psw:StrLen/binary>> = Bin,
            io:format("~p,~p,~p~n",[Command_ID,UserId,Psw]),
            StrPsw = binary_to_list(Psw),
            if
                Command_ID == ?LOGIN_COMMAND_ID ->
                    case is_auth(UserId,StrPsw,TabId) of
                        {auth,ok} -> 
                            %向客户端返回通知，完成协议过程
                            case
                                auth_feedback(UserId,TabId,Socket) of
                                true ->  {login,ok};
                                false -> {login,error}
                            end;
                        _Other -> {login,error}
                    end;
                true -> {login,error}
            end;
            %io:format("~p,~p,~p~n",[Command_ID,UserId,Psw]),
        _Other ->
            io:format("auth_login error"),
            {login,error}
    end.

auth_feedback(UserId,TabId,Socket) ->
    {_UserTab,UserInfo,_Online} = TabId,
    case ets:lookup(UserInfo,UserId) of
        [{_,Name,_,_,_}] -> 
            StrLen = string:len(Name),
            Message_Len = 16+16+16+32+16+StrLen,
            SendBin = << Message_Len:16,?LOGIN_COMMAND_ID:16,?SUCCEED:16,
                UserId:32,StrLen:16,(list_to_binary(Name))/binary >>,
            gen_tcp:send(Socket,SendBin),
            true;
        _Other -> false
    end.

            
%验证用户名和密码
is_auth(UserId,Psw,TabId) ->
    {UserTab,_UserInfo,_Online} = TabId,
    case ets:lookup(UserTab,UserId) of 
        [{_,Psw}] -> 
            {auth,ok};
        _Other -> {auth,error}
    end.
    

loop(Socket,ManagerClientPid) ->
    receive
        {tcp,Socket,Bin} ->
            Str =  binary_to_term(Bin),
            ManagerClientPid ! {send,Bin},
            loop(Socket,ManagerClientPid);
        {tcp_closed,Socket} ->
            io:format("Server socket closed~n")
    end.

manage_client(ClientList) ->
    receive
        {connected,Socket} ->
            manage_client([Socket|ClientList]);
        {disconnected,Socket} ->
            NewList = lists:delete(Socket,ClientList),
            manage_client(NewList);
        {send,Bin} ->
            send_data(ClientList,Bin),
            manage_client(ClientList);
        Other ->
            manage_client(ClientList)
    end.


send_data(SocketList,Bin) ->
    lists:foreach(fun(Socket) ->
                gen_tcp:send(Socket,Bin)
                  end,
                  SocketList).
