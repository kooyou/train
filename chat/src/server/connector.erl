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
-include("debug_data.hrl").

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================



manage_one_connector(Socket,DataPid) ->
    %进入消息循环，标示为未登录
    loop(Socket,DataPid,false).


    
%接收每个客户端的消息
%每个客户端的接收循环都拥有一个客户端管理进程的ID
%以便将消息通过客户端管理进程进行广播
loop(Socket,DataPid,Is_auth) ->
    receive
        {login,succeed} ->
            %登录成功，标示为ture
            loop(Socket,DataPid,true);
        {login,false} ->
            %登录失败，直接退出
            void;
        {tcp,Socket,Bin} ->
            %获取协议命令码
            {CmdCode,MsgLen} = protocol_pro:get_protocol_cmd(Bin),
            case Is_auth of
                true ->
                    %根据命令码进行分配处理
                    dispatcher(CmdCode,MsgLen,Bin,Socket,
                        DataPid),
                    loop(Socket,DataPid,Is_auth);

                false ->
                    if CmdCode =:= ?LOGIN_CMD_ID -> %登录
                            dispatcher(CmdCode,MsgLen,Bin,Socket,
                                DataPid);
                       CmdCode =:= ?REGISTER_CMD_ID -> %注册
                            dispatcher(CmdCode,MsgLen,Bin,Socket,
                                DataPid);
                       true -> loop(Socket,DataPid,Is_auth)

                    end,
                    loop(Socket,DataPid,Is_auth)
            end;
        {tcp_closed,Socket} ->
            %下线，从客户端列表删除Socket
            %ManagerClientPid ! {disconnected,Socket},
            %客户端下线，将客户端的Socket从列表删除
            DataPid ! {del_user_info,Socket},
            DataPid ! {del_online,Socket},
            io:format("Client socket closed!~n")
    end.


%根据协议命令对消息进行分配处理
dispatcher(Cmdcode,MsgLen,Bin,Socket,DataPid) ->
    %首先完成对数据包的协议分解
    GetData = protocol_pro:cmdcode_match(Cmdcode,MsgLen,Bin),

    case Cmdcode of
        ?REGISTER_CMD_ID -> register_handler(GetData,Socket,DataPid);
        ?LOGIN_CMD_ID -> login_handler(GetData,Socket,DataPid);
        ?WHOONLINE_CMD_ID -> who_online(GetData,Socket,DataPid);
        ?CHAT_SEND_CMD_ID -> chat_send(GetData,Socket,DataPid);
        ?CHAT_REV_CMD_ID -> chat_rev(GetData,Socket,DataPid);
        ?LOGIN_TIMES_CMD_ID -> login_times(GetData,Socket,DataPid);
        ?CHAT_TIMES_CMD_ID -> chat_times(GetData,Socket,DataPid)
    end.

%应答注册请求
register_handler(Data,Socket,DataPid) ->
    {_MsgLen,_CmdID,Name,Psw} = Data,
    case chat_data:get_user_f_name(Name) of
        [] -> 
            NewId = chat_data:get_new_id(),
            chat_data:add_user(NewId,Name,Psw),
            SendData = {?SUCCEED,NewId},
            SendBin = protocol_pro:cmdcode_pack(?REGISTER_CMD_ID,SendData),
            sendto(Socket,SendBin);
        _Other ->
            SendData = {?FALSE,0},
            SendBin = protocol_pro:cmdcode_pack(?REGISTER_CMD_ID,SendData),
            sendto(Socket,SendBin)
    end.

%应答登录处理
login_handler(Data,Socket,DataPid) ->
    %验证登录的用户名和密码
    case auth_login(DataPid,Data,Socket) of
        {login,ok,UserId} ->

            %登录成功，修改用户相应的数据表信息
            %向ets写入用户信息
            DataPid ! {add_user_info,UserId},
            %增加一次登录次数
            DataPid ! {add_login_times,UserId},
            %修改最后一次登录时间
            DataPid ! {update_lastlogin,UserId},

            %向在线表写入数据
            DataPid ! {get_user_name,UserId,self()},
            receive
                [{_,Name,_,_,_}] -> 
                    DataPid ! {add_online,Socket,UserId,Name};
                Other -> io:format("has some problem!~p,~p~n",[UserId,Other])
            end,

            %检测警报：tooMuchClient
            DataPid ! {get_online_num,self()},
            receive
                {online,Num} -> 
                    if
                        Num > 1000 ->
                            alarm_handler:set_alarm(tooMuchClient);
                        true -> void
                    end;
                _Other ->
                    alarm_handler:set_alaram()
            end,


            %登录成功，开始接收消息
            self() ! {login,succeed};
            
        {login,error} ->
            self() ! {login,false}
    end.
    

%验证登录
auth_login(DataPid,Data,Socket) ->
            {_MesLen,Command_ID,UserId,StrPsw} = Data,
            if
                Command_ID == ?LOGIN_CMD_ID ->
                    case is_auth(UserId,StrPsw,DataPid) of
                        {auth,ok} -> 
                            %向客户端返回通知，完成协议过程
                            case
                                auth_feedback(UserId,DataPid,Socket,true) of
                                true ->  {login,ok,UserId};
                                false -> {login,error}
                            end;
                        _Other -> 
                            auth_feedback(UserId,DataPid,Socket,false),
                            {login,error}
                    end;
                true -> {login,error}
            end.

%应答客户端的登录请求
auth_feedback(UserId,DataPid,Socket,Is_auth) ->
    %{_UserTab,UserInfo,_Online} = TabId,
    case Is_auth of
        true ->
            %对应答协议包封包
            Data = {?SUCCEED,UserId,""},
            SendBin = protocol_pro:cmdcode_pack(?LOGIN_CMD_ID,Data),
            sendto(Socket,SendBin),
            true;

        false ->
            Data = {?FALSE,0,""},
            SendBin = protocol_pro:cmdcode_pack(?LOGIN_CMD_ID,Data),
            sendto(Socket,SendBin),
            false
    end.

            
%验证用户名和密码
is_auth(UserId,Psw,DataPid) ->
    DataPid ! {get_user_psw,UserId,self()},
    receive
        [{_,Psw}] -> 
            {auth,ok};
        _Other -> {auth,error}
    end.


%应答查看在线人数请求
who_online(BinData,Socket,DataPid) ->
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

%应答聊天请求
chat_send(BinData,Socket,DataPid) ->

    {_Message_Len,_Command_ID,_Send_User_Id,_Send_User_Name,
        _Receive_User_Id,_Send_Data_Type,Send_Data} = BinData,

    %首先查找发送者信息，然后在消息中添加发送者信息
    DataPid ! {get_online_name,Socket,self()},
    receive
        [{_,UserId,UserName}] ->
             %对应答内容进行封包处理
             Data = {UserId,UserName,?SUCCEED,Send_Data},
             SendBin = protocol_pro:cmdcode_pack(?CHAT_REV_CMD_ID,Data),
             
             %广播消息(添加了发送者信息)
             %ManagerClientPid ! {send,SendBin},
             io:format("all_online:befor socket~n"),
             DataPid ! {all_online_socket,self()},
             io:format("all_online:socket~n"),
             receive
                {all_online,ClientList} ->
                    io:format("all_online:~p~n",ClientList),
                    %广播信息
                    send_data_to_list(ClientList,SendBin);
                _Other -> io:format("all_online:nonononono~n")
             end,

             %向数据库添加聊天次数
             DataPid ! {add_chat_times,UserId};
       _Other -> 
             void
    end.



chat_rev(_BinData,_Socket,_DataPid) ->
    void.

%查看登录次数
login_times(RevData,Socket,DataPid) ->
    {   _Message_Len,
        _Command_Id,
        User_Id } = RevData,
    DataPid ! {get_login_times,User_Id,self()},
    receive
        {login_times,LoginTimes} -> 
            Data = {0,LoginTimes},
            SendBin = protocol_pro:cmdcode_pack(?LOGIN_TIMES_CMD_ID,Data),
            sendto(Socket,SendBin);
        _Other -> io:format("connector login_times error!~n")
    end.

%查看聊天次数
chat_times(RevData,Socket,DataPid) ->
     {  _Message_Len,
        _Command_Id,
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

%对客户端列表进行数据广播
send_data_to_list(SocketList,Bin) ->
    lists:foreach(fun(Socket) ->
                gen_tcp:send(Socket,Bin)
                  end,
                  SocketList).
