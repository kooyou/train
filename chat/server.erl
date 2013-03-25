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
    spawn(fun() -> par_connect(Listen,ManagerClientPid,DataPid) end).



%并行连接
par_connect(Listen,ManagerClientPid,DataPid) ->
    {ok,Socket} = gen_tcp:accept(Listen),
    
    %创建新进程进行新客户端的连接
    spawn(fun() -> par_connect(Listen,ManagerClientPid,DataPid) end),

    %验证登录的用户名和密码
    case auth_login(DataPid) of
        {login,ok,UserId} -> 
            ManagerClientPid ! {connected,Socket,UserId},
            %登录成功，开始接收消息
            loop(Socket,ManagerClientPid,DataPid);
        {login,error} ->
            void
    end.

%验证登录
auth_login(DataPid) ->
    receive
        {tcp,Socket,Bin} ->
            %登录协议解包
            <<Message_Len:16,Command_ID:16,
            UserId:32,StrLen:16,Psw:StrLen/binary>> = Bin,
            %io:format("~p,~p,~p~n",[Command_ID,UserId,Psw]),
            StrPsw = binary_to_list(Psw),
            if
                Command_ID == ?LOGIN_COMMAND_ID ->
                    case is_auth(UserId,StrPsw,DataPid) of
                        {auth,ok} -> 
                            %向客户端返回通知，完成协议过程
                            case
                                auth_feedback(UserId,DataPid,Socket) of
                                true ->  {login,ok,UserId};
                                false -> {login,error}
                            end;
                        _Other -> {login,error}
                    end;
                true -> {login,error}
            end;
        _Other ->
            io:format("auth_login error"),
            {login,error}
    end.

%应答客户端的登录请求
auth_feedback(UserId,DataPid,Socket) ->
    %{_UserTab,UserInfo,_Online} = TabId,
    DataPid ! {get_user_name,UserId,self()},
    receive
        [{_,Name,_,_,_}] -> 
            %对应答协议包封包
            StrLen = string:len(Name),
            Message_Len = 16+16+16+32+16+StrLen,
            SendBin = << Message_Len:16,?LOGIN_COMMAND_ID:16,?SUCCEED:16,
                UserId:32,StrLen:16,(list_to_binary(Name))/binary >>,
            gen_tcp:send(Socket,SendBin),
            true;

        _Other -> false
    end.

            
%验证用户名和密码
is_auth(UserId,Psw,DataPid) ->
    %{UserTab,_UserInfo,_Online} = TabId,
    DataPid ! {get_user_psw,UserId,self()},
    receive
        [{_,Psw}] -> 
            {auth,ok};
        _Other -> {auth,error}
    end.
    
%接收每个客户端的消息
%每个客户端的接收循环都拥有一个客户端管理进程的ID
%以便将消息通过客户端管理进程进行广播
loop(Socket,ManagerClientPid,DataPid) ->
    receive
        {tcp,Socket,Bin} ->
            Str =  binary_to_term(Bin),
            %添加发送者信息
            DataPid ! {get_online_name,Socket,self()},
            receive
                [{_,UserName}] ->
                    io:format("get_online: ~p~n",[UserName]),
                    TemStr = string:concat(UserName," : "),
                    SendStr = string:concat(TemStr,Str),
                    SendBin = list_to_binary(SendStr);
                _Other -> SendBin = Bin
            end,
            %广播消息
            ManagerClientPid ! {send,SendBin},
            loop(Socket,ManagerClientPid,DataPid);
        {tcp_closed,Socket} ->
            %下线，从客户端列表删除Socket
            ManagerClientPid ! {disconnected,Socket},
            io:format("Client socket closed~n")
    end.

%管理客户端
manage_client(ClientList,DataPid) ->
    receive
        {connected,Socket,UserId} ->
            %客户端登录成功，将客户端的Socket加入列表
            manage_client([Socket|ClientList],DataPid),
            %向在线表写入数据
            DataPid ! {get_user_name,UserId,self()},
            io:format("adfasd"),
            receive
                [_,Name,_,_,_] -> DataPid ! {add_online,Socket,Name};
                _Other -> void
            end;
        {disconnected,Socket} ->
            %客户端下线，将客户端的Socket从列表删除
            NewList = lists:delete(Socket,ClientList),
            manage_client(NewList,DataPid);
        {send,Bin} ->
            %广播信息
            send_data(ClientList,Bin),
            manage_client(ClientList,DataPid);
        Other ->
            manage_client(ClientList,DataPid)
    end.


send_data(SocketList,Bin) ->
    lists:foreach(fun(Socket) ->
                gen_tcp:send(Socket,Bin)
                  end,
                  SocketList).
