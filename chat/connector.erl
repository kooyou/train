%%%------------------------------------
%%% @Module  : connector
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/26
%%% @Description:  负责管理服务器与客户端的一个连接，实际就是管理一个客户端
%%%------------------------------------

-module(connector).
-compile(export_all).
-include("protocol.hrl").

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================



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
                Command_ID == ?LOGING_CMD_ID ->
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
            SendBin = << Message_Len:16,?LOGING_CMD_ID:16,?SUCCEED:16,
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
            %根据命令码进行分配处理
            dispatcher(CmdCode,MsgLen,Bin,Socket,DataPid,ManagerClientPid),

            loop(Socket,ManagerClientPid,DataPid);
        {tcp_closed,Socket} ->
            %下线，从客户端列表删除Socket
            ManagerClientPid ! {disconnected,Socket},
            io:format("Client socket closed~n")
    end.


%根据协议命令对消息进行分配处理
dispatcher(Cmdcode,MsgLen,Bin,Socket,DataPid,ManagerClientPid) ->
    %首先完成对数据包的协议分解
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

%应答登录处理
login_handler(BinData,Socket,DataPid,ManagerClientPid) ->
    void.

%应答查看在线人数请求
who_online(BinData,Socket,DataPid,ManagerClientPid) ->
    
    {_Message_Len,Command_Id} = BinData,
    DataPid ! {get_online_num,self()},
    receive
        {online,N} -> 
            ResponseData = {0,N},
            ResponseBin = 
                    protocol_pro:cmdcode_pack(Command_Id,ResponseData),
            sendto(Socket,ResponseBin);
        _Other -> void
    end.

%应答查看在线朋友请求
fnd_online(BinData,Socket,DataPid,ManagerClientPid) ->
    void.

%应答聊天请求
chat_send(BinData,Socket,DataPid,ManagerClientPid) ->

    {Message_Len,Command_ID,Send_User_Id,Send_User_Name,
        Receive_User_Id,Send_Data_Type,Send_Data} = BinData,

    %首先查找发送者信息，然后在消息中添加发送者信息
    DataPid ! {get_online_name,Socket,self()},
    receive
        [{_,UserId,UserName}] ->

             %对应答内容进行封包处理
             Data = {UserId,UserName,?SUCCEED,Send_Data},
             SendBin = protocol_pro:cmdcode_pack(?CHAT_REV_CMD_ID,Data),
             
             %广播消息(添加了发送者信息)
             ManagerClientPid ! {send,SendBin},

             %向数据库添加聊天次数
             DataPid ! {add_chat_times,UserId};
       _Other -> 
             %广播消息
             %ManagerClientPid ! {send,term_to_binary(Send_Data)}
             void
    end.



chat_rev(BinData,Socket,DataPid,ManagerClientPid) ->
    void.

login_times(RevData,Socket,DataPid,ManagerClientPid) ->
    {   _Message_Len,
        Command_Id,
        User_Id } = RevData,
    DataPid ! {get_login_times,User_Id,self()},
    receive
        {login_times,LoginTimes} -> 
            Data = {0,LoginTimes},
            SendBin = protocol_pro:cmdcode_pack(?LOGIN_TIMES_CMD_ID,Data),
            sendto(Socket,SendBin);
        _Other -> io:format("connector login_times error!~n")
    end.

chat_times(RevData,Socket,DataPid,ManagerClientPid) ->
     {  _Message_Len,
        Command_Id,
        User_Id } = RevData,
    DataPid ! {get_chat_times,User_Id,self()},
    receive
        {chat_times,ChatTimes} -> 
            Data = {0,ChatTimes},
            SendBin = protocol_pro:cmdcode_pack(?CHAT_TIMES_CMD_ID,Data),
            sendto(Socket,SendBin);
        _Other -> io:format("connector chat_times error!~n")
    end.

sendto(Socket,Bin) ->
    gen_tcp:send(Socket,Bin).
