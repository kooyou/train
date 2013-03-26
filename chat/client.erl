%%%------------------------------------
%%% @Module  : client
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: 聊天客户端
%%%------------------------------------
-module(client).
-compile(export_all).

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

%%=========================================================================
%%宏
%%=========================================================================
-define(SUCCEED,1).  %请求成功
-define(LOGING_CMD_ID,10001).         %登录请求命令码
-define(WHOONLINE_CMD_ID,10002).      %查看在线请求命令码
-define(FNDONLINE_CMD_ID,10003).      %查看在线朋友请求命令码  
-define(CHAT_SEND_CMD_ID,10004).      %发送聊天信息请求命令码
-define(CHAT_REV_CMD_ID,10005).       %接收聊天信息请求命令码
-define(LOGIN_TIMES_CMD_ID,10006).    %查看登录次数请求命令码
-define(CHAT_TIMES_CMD_ID,10007).     %查看聊天次数请求命令码


-define(MSG_HEAD_LEN,16).             %信息头信息长度
-define(CMD_LEN,16).                  %信息头命令码长度

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
    _Pid = spawn(fun() -> connect(S,UserRecord) end),

    %子进程会返回登录结果消息
     receive
         {connected,_Socket,NewUserRecord} ->
            %登录成功
            io:format("login success!~n"),
            io:format("please type the command:~n"),
            %进入等待命令模式
            wait_for_cmd(NewUserRecord);
         _Other ->
             error
     end.

wait_for_cmd(UserRecord) ->
    CmdStr = io:get_line("chat>"),
    {Cmd,Data} = analysis_cmd(CmdStr),
    case Cmd of
        "chat" -> send(UserRecord,Data);
        _Other -> analysis_cmd(Cmd)
    end,
    wait_for_cmd(UserRecord).

analysis_cmd(String) ->
    %第一个空格前的字符串为命令
    Index = string:chr(String,$ ),
    StrLen = string:len(String),
    if
        (Index > 0) andalso (Index < StrLen )->
            SubStr = string:sub_string(String,1,Index-1),
            LastStr = string:sub_string(String,Index+1,StrLen-2),
            io:format("~p,~p~n",[SubStr,LastStr]),
            {SubStr,LastStr};
        true -> {"error",""}
    end.


%发送信息
senddata(Socket,Bin) ->
    gen_tcp:send(Socket,Bin).

%查看在线人数
who_online(Socket) ->
    Message_Len = ?MSG_HEAD_LEN + ?CMD_LEN ,
    %封装请求协议
    SendBin = <<Message_Len:16,?WHOONLINE_CMD_ID:16>>,
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

                io:format("~p~n",[SendBin]),

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
                    loop(Socket);
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
loop(Socket) ->
    receive
        {tcp,Socket,Bin} ->
            Val= binary_to_term(Bin),
                io:format("Msg-> ~p~n",[Val]);
        _Other ->
            io:format("client rev error data!")
    end,
    loop(Socket).

