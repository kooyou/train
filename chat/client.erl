%%%------------------------------------
%%% @Module  : client
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: 聊天客户端
%%%------------------------------------
-module(client).
-compile(export_all).
-include("protocol.hrl").

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

%%=========================================================================
%%宏
%%=========================================================================

-record(user,{
    id,             %用户ID
    name,           %用户名称
    passwd,         %用户登录密码
    login_times,    %登录次数
    chat_times,     %聊天次数
    last_login,     %最后一次登录时间
    socket          %连接的socket
}).

%登录到服务器
%过程：连接->登录验证
login(UserId,Psw) ->

    %创建当前用户记录
    UserRecord = #user{id=UserId,passwd=Psw},

    %连接到服务器
    S = self(),
    Pid = spawn(fun() -> connect(S,UserRecord) end),

    %子进程会返回登录结果消息
     receive
         {connected,_Socket,NewUserRecord} ->
            %登录成功
            io:format("login success!~n"),
            io:format("please type the command:~n"),
            %进入等待命令模式
            wait_for_cmd(NewUserRecord,Pid);
         _Other ->
             error
     end.

 %循环等待用户命令
wait_for_cmd(UserRecord,Pid) ->
    CmdStr = io:get_line("chat>"),
    {Cmd,Data} = analysis_cmd(CmdStr),
    %io:format("~p,~p~n",[Cmd,Data]),
    case Cmd of
        "chat" -> 
            send(UserRecord,Data),
            wait_for_cmd(UserRecord,Pid);
        "online" -> 
            who_online(UserRecord),
            wait_for_cmd(UserRecord,Pid);
        "logintimes" -> 
            login_times(UserRecord),
            wait_for_cmd(UserRecord,Pid);
        "chattimes" -> 
            chat_times(UserRecord),
            wait_for_cmd(UserRecord,Pid);
        "quit" -> 
            quit(UserRecord,Pid);
        _Other -> io:format("bad command!~n"),
            wait_for_cmd(UserRecord,Pid)
    end.
    

analysis_cmd(String) ->
    %第一个空格前的字符串为命令
    Index = string:chr(String,$ ),
    StrLen = string:len(String),
    if
        (Index > 0) andalso (Index < StrLen )->
            SubStr = string:sub_string(String,1,Index-1),
            LastStr = string:sub_string(String,Index+1,StrLen-1),
            {SubStr,LastStr};
        true -> { string:sub_string(String,1,string:len(String)-1),""}
    end.


%发送信息
senddata(Socket,Bin) ->
    gen_tcp:send(Socket,Bin).

%退出
quit(UserRecord,RevPid) ->
    Socket = UserRecord#user.socket,
    gen_tcp:close(Socket),
    RevPid ! stop. %退出接收进程

%查看聊天次数
chat_times(UserRecord) ->
    Socket = UserRecord#user.socket,
    UserId = UserRecord#user.id,
    SendData = {UserId},
    SendBin = c_protocol_pro:cmdcode_match(
        ?CHAT_TIMES_CMD_ID,0,SendData,false),
    senddata(Socket,SendBin).


%查看登录次数
login_times(UserRecord) ->
    Socket = UserRecord#user.socket,
    UserId = UserRecord#user.id,
    SendData = {UserId},
    SendBin = c_protocol_pro:cmdcode_match(
        ?LOGIN_TIMES_CMD_ID,0,SendData,false),
    senddata(Socket,SendBin).

%查看在线人数
who_online(UserRecord) ->
    Socket = UserRecord#user.socket,

    SendData = {},
    SendBin = c_protocol_pro:cmdcode_match(?WHOONLINE_CMD_ID,0,SendData,false),
    senddata(Socket,SendBin).

%发送聊天信息
send(UserRecord,SendData) ->
    Socket = UserRecord#user.socket,

    %封装协议包
    UserName = binary_to_list(UserRecord#user.name),
    NameLen = string:len(UserName),
    StrLen = string:len(SendData),
    Message_Len = ?MSG_HEAD_LEN + ?CMD_LEN + 32 + 16 +
                    NameLen * 8 + 32 + 16 + StrLen * 8,

    SendBin = <<Message_Len:16,             %消息长度
                ?CHAT_SEND_CMD_ID:16,       %消息命令码
                (UserRecord#user.id):32,      %发送信息的用户ID
                NameLen:16,                 %用户名称长度
                                            %发送信息的用户名称
                (UserRecord#user.name)/binary,
                0:32,                       %接收信息的用户ID
                0:16,                       %消息数据类型string
                StrLen:16,                  %信息体长度
                                            %信息体
                (list_to_binary(SendData))/binary>>,

    gen_tcp:send(Socket,SendBin).

 %连接并登录
connect(Parent,UserRecord) ->
    UserId = UserRecord#user.id,
    Psw = UserRecord#user.passwd,
    case gen_tcp:connect("localhost",2345,[binary,{packet,4}]) of
        {ok,Socket} ->
            case try_to_login(UserId,Psw,Socket) of
                {true,UserName} -> 
                    %登录成功，向父进程发送登录成功消息
                    UserRecordNew = 
                            UserRecord#user{name=UserName,socket=Socket},
                    Parent ! {connected,Socket,UserRecordNew},
                    %进入接收消息循环
                    loop(Socket,UserRecord);
                false ->
                    Parent ! error
            end;
        _Other ->
            Parent ! error
    end.


%登录到服务器
try_to_login(UserId,Psw,Socket) ->
    %对请求消息进行协议封装
    Message_Len = 16 + 16 + 32 + 16 + string:len(Psw) * 8, 
    StrLen = string:len(Psw),
    SendBin = <<Message_Len:16,?LOGING_CMD_ID:16,UserId:32,
    StrLen:16,(list_to_binary(Psw))/binary>>,
    gen_tcp:send(Socket,SendBin),
    %接收请求应答，并解包分析是否登录成功
    receive
        {tcp,Socket,Bin} ->
            <<_RevMessage_Len:16,_Command_ID:16,Is_Succeed:16,
              _User_Id:32,StringLen:16,UserName:StringLen/binary>> = Bin,
                     
            if
                Is_Succeed =:= ?SUCCEED ->
                    {true,UserName};
                true -> false
            end;
        _Other ->
            false
    end.

%循环接收消息
loop(Socket,UserRecord) ->
    receive
        {tcp,Socket,Bin} ->
            {Cmdcode,MsgLen} = c_protocol_pro:get_protocol_cmd(Bin),
            RevData = c_protocol_pro:cmdcode_match(Cmdcode,MsgLen,Bin,true),
            msg_handler(Cmdcode,MsgLen,RevData,UserRecord),
            
            loop(Socket,UserRecord);
        stop -> void;
        _Other ->
            io:format("client rev error data!~n"),
            loop(Socket,UserRecord)
    end.


%根据消息命令码对消息包进行相应处理
msg_handler(Cmdcode,MsgLen,Bin,UserRecord) ->
    case Cmdcode of
       % ?LOGING_CMD_ID -> login_bag_parse(MsgLen,Bin);
        ?WHOONLINE_CMD_ID -> whoonline_rev(MsgLen,Bin);
       % ?FNDONLINE_CMD_ID -> fndonline_bag_parse(MsgLen,Bin);
       % ?CHAT_SEND_CMD_ID -> chat_send_bag_parse(MsgLen,Bin);
        ?CHAT_REV_CMD_ID -> chat_rev(MsgLen,Bin,UserRecord);
        ?LOGIN_TIMES_CMD_ID -> logintimes_rev(MsgLen,Bin);
        ?CHAT_TIMES_CMD_ID -> chat_times_rev(MsgLen,Bin);
       _Other -> io:format("client error command ID : ~p",[Cmdcode])
    end.

whoonline_rev(_MsgLen,Bin) ->
    {_Message_Len,
     _Command_ID,
     _Result,
     OnlineNum} = Bin,
    io:format("The number of the online is ~p.~n",[OnlineNum]),
    void.


%对接收到的聊天信息进行处理
chat_rev(_MsgLen,RevData,UserRecord) ->
    {_Message_Len,
     _Command_ID,
     Send_User_Id,
     Send_User_Name,
     _Send_Data_Type,
     Send_Data} = RevData,
    
     if
        Send_User_Id =:= UserRecord#user.id ->
            io:format("**Msg** I(~p) say:~s~n",[Send_User_Id,Send_Data]);
        true ->
            io:format("**Msg** ~s(~p) say:~s~n",[Send_User_Name,
                         Send_User_Id,Send_Data])
    end.


logintimes_rev(_MsgLen,RevData) ->
    {_Message_Len,
    _Command_Id,
    _Result,
    Login_Times} = RevData,
    io:format("Login times is ~p~n",[Login_Times]),
    void.

chat_times_rev(_MsgLen,RevData) ->
    {_Message_Len,
    _Command_Id,
    _Result,
    Chat_Times} = RevData,
    io:format("Chat times is ~p~n",[Chat_Times]),
    void.
