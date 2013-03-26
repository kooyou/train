%%%------------------------------------
%%% @Module  : connector
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/26
%%% @Description:  负责管理服务器与客户端的一个连接，实际就是管理一个客户端
%%%------------------------------------

-module(connector).
-compile(export_all).


%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

-define(LOGIN_COMMAND_ID,10001).
-define(SUCCEED,1).
-define(FALSE,1).

-define(LOGING_CMD_ID,10001).         %登录请求命令码
-define(WHOONLINE_CMD_ID,10002).      %查看在线请求命令码
-define(FNDONLINE_CMD_ID,10003).      %查看在线朋友请求命令码  
-define(CHAT_SEND_CMD_ID,10004).      %发送聊天信息请求命令码
-define(CHAT_REV_CMD_ID,10005).       %接收聊天信息请求命令码
-define(LOGIN_TIMES_CMD_ID,10006).    %查看登录次数请求命令码
-define(CHAT_TIMES_CMD_ID,10007).     %查看聊天次数请求命令码


manage_one_connector(Socket,ManagerClientPid,DataPid) ->
    %验证登录的用户名和密码
    case auth_login(DataPid) of
        {login,ok,UserId} -> 
            ManagerClientPid ! {connected,Socket,UserId},

            %登录成功，修改用户相应的数据表信息
            %增加一次登录次数
            DataPid ! {add_login_times,UserId},
            %修改最后一次登录时间
            DataPid ! {update_lastlogin,UserId},

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
            <<_Message_Len:16,Command_ID:16,
            UserId:32,StrLen:16,Psw:StrLen/binary>> = Bin,
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

            %获取协议命令码
            {CmdCode,MsgLen} = protocol_pro:get_protocol_cmd(Bin),
            %根据命令码进行处理分流
            cmdcode_match(CmdCode,MsgLen,Bin,Socket,DataPid,ManagerClientPid),
            
           
            loop(Socket,ManagerClientPid,DataPid);
        {tcp_closed,Socket} ->
            %下线，从客户端列表删除Socket
            ManagerClientPid ! {disconnected,Socket},
            io:format("Client socket closed~n")
    end.


%根据协议命令对消息进行分配处理
cmdcode_match(Cmdcode,MsgLen,Bin,Socket,DataPid,ManagerClientPid) ->
    GetData = protocol_pro:cmdcode_match(Cmdcode,MsgLen,Bin),
    case Cmdcode of
        ?LOGING_CMD_ID -> login_handler(GetData,Socket,DataPid,ManagerClientPid);
        ?WHOONLINE_CMD_ID -> who_online(GetData,Socket,DataPid,ManagerClientPid);
        ?FNDONLINE_CMD_ID -> fnd_online(GetData,Socket,DataPid,ManagerClientPid);
        ?CHAT_SEND_CMD_ID -> chat_send(GetData,Socket,DataPid,ManagerClientPid);
        ?CHAT_REV_CMD_ID -> chat_rev(GetData,Socket,DataPid,ManagerClientPid);
        ?LOGIN_TIMES_CMD_ID -> login_times(GetData,Socket,DataPid,ManagerClientPid);
        ?CHAT_TIMES_CMD_ID -> chat_times(GetData,Socket,DataPid,ManagerClientPid)
    end.

login_handler(BinData,Socket,DataPid,ManagerClientPid) ->
    void.

who_online(BinData,Socket,DataPid,ManagerClientPid) ->
    void.

fnd_online(BinData,Socket,DataPid,ManagerClientPid) ->
    void.

chat_send(BinData,Socket,DataPid,ManagerClientPid) ->
    {Message_Len,Command_ID,Send_User_Id,Send_User_Name,
        Receive_User_Id,Send_Data_Type,Send_Data} = BinData,
    %首先查找发送者信息，然后在消息中添加发送者信息
    DataPid ! {get_online_name,Socket,self()},
    receive
        [{_,_UserId,UserName}] ->
             TemStr = string:concat(UserName," : "),
             SendStr = string:concat(TemStr,Send_Data),
             %io:format("~p~n",[SendStr]),
             SendBin = term_to_binary(SendStr),
             %广播消息(添加了发送者信息)
             ManagerClientPid ! {send,SendBin};
       _Other -> 
             %广播消息
             ManagerClientPid ! {send,term_to_binary(Send_Data)}
    end.

chat_rev(BinData,Socket,DataPid,ManagerClientPid) ->
    void.

login_times(BinData,Socket,DataPid,ManagerClientPid) ->
    void.

chat_times(BinData,Socket,DataPid,ManagerClientPid) ->
    void.

