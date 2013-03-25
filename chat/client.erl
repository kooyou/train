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
%%数据
%%=========================================================================
-define(SUCCEED,1).
-define(LOGING_COMMAND_ID,10001).

%登录到服务器
%过程：连接->登录验证
login(UserId,Psw) ->
    %连接到服务器
    S = self(),
    _Pid = spawn(fun() -> connect(S,UserId,Psw) end),

    %子进程会返回登录结果消息
     receive
         {connected,Socket} ->
             %登录成功，返回Socket以便调用
            Socket;
         _Other ->
             error
     end.

%发送聊天信息
send(Socket,Str) ->
     gen_tcp:send(Socket,term_to_binary(Str)).

 %连接并登录
connect(Parent,UserId,Psw) ->
    case gen_tcp:connect("localhost",2345,[binary,{packet,4}]) of
        {ok,Socket} ->
            case try_to_login(UserId,Psw,Socket) of
                true -> 
                    %登录成功，向父进程发送登录成功消息
                    Parent ! {connected,Socket},
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
    Message_Len = 16 + 16 + 32 + 16 + string:len(Psw),
    StrLen = string:len(Psw),
    SendBin = <<Message_Len:16,?LOGING_COMMAND_ID:16,UserId:32,
    StrLen:16,(list_to_binary(Psw))/binary>>,
    gen_tcp:send(Socket,SendBin),
    %接收请求应答，并解包分析是否登录成功
    receive
        {tcp,Socket,Bin} ->
            <<_RevMessage_Len:16,_Command_ID:16,Is_Succeed:16,
              _User_Id:32,StringLen:16,_UserName:StringLen/binary>> = Bin,
                     
            if
                Is_Succeed =:= ?SUCCEED ->
                    true;
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
                io:format("client result = ~p~n",[Val]);
        _Other ->
            io:format("client rev error data!")
    end,
    loop(Socket).

