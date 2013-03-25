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

login(UserId,Psw) ->
    %连接到服务器
    S = self(),
    _Pid = spawn(fun() -> connect(S,UserId,Psw) end),
     receive
         {connected,Socket} ->
            Socket;
         _Other ->
             error
     end.
 send(Socket,Str) ->
     gen_tcp:send(Socket,term_to_binary(Str)).

connect(Parent,UserId,Psw) ->
    case gen_tcp:connect("localhost",2345,[binary,{packet,4}]) of
        {ok,Socket} ->
            case try_to_login(UserId,Psw,Socket) of
                true ->
                    Parent ! {connected,Socket},
                    loop(Socket);
                false ->
                    Parent ! error
            end;
        _Other ->
            Parent ! error
    end.
    %ok = gen_tcp:send(Socket,term_to_binary(Str)),


try_to_login(UserId,Psw,Socket) ->
     Message_Len = 16 + 16 + 32 + 16 + string:len(Psw),
             StrLen = string:len(Psw),
             SendBin = <<Message_Len:16,?LOGING_COMMAND_ID:16,UserId:32,
             StrLen:16,(list_to_binary(Psw))/binary>>,
             gen_tcp:send(Socket,SendBin),
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

loop(Socket) ->
        receive
            {tcp,Socket,Bin} ->
                Val= binary_to_term(Bin),
                io:format("client result = ~p~n",[Val]);
            _Other ->
                io:format("client rev error data!")
        end,
        loop(Socket).

