%%%------------------------------
%%%@Module :chat_client
%%%@Author :fengzhenlin
%%%@Email  :535826356@qq.com
%%%@Created :2013.3.24
%%%@Description :聊天客户端
%%%----------------------------

-module(chat_client).
-compile(export_all).

login(UserId,Psw) ->
	S = self(),
	Channel = spawn(fun() -> connect(S) end),
	%等待连接结果
	receive
		{ok,login,MM} -> %连接成功，尝试登录
			spawn(fun() -> try_login(UserId,Psw,MM,S) end),
			%等待登录结果
			receive
				{login,ok} -> io:format("login ok!~n"),
					RevPid = spawn(fun() -> rev() end),
					lib_chan_mm:controller(MM,RevPid),
					%返回用于发送信息的Pid
					{ok,MM};
				{login,error} -> io:format("error:false to login!~n")
			end;
		{error,login} ->
			io:format("chat_client login error!~n")
	end.

send(MM,UserNick,Str) ->
	lib_chan_mm:send(MM,{Str,UserNick}).
%连接到服务器
connect(Parent) ->
	case lib_chan:connect("localhost",2223,chat,"AsDT67aQ",[]) of
		{ok,MM} -> Parent ! {ok,login,MM};
		{error,_Why} -> Parent ! {error,login}
	end.

%登录
try_login(UserId,Psw,MM,Parent) ->
	lib_chan_mm:controller(MM,self()),
	lib_chan_mm:send(MM,{login,UserId,Psw}),
	receive
		{chan,MM,{login,ok}} ->
			Parent ! {login, ok};
			
		{chan,MM,{login,error}} ->
			Parent ! {login,error};
			
		Other ->
			Parent ! {login,error}
	end.

rev() ->
	receive
		{chan,MM,{chat,Str}} ->
			io:format("~p~n",[Str]),
			rev();
		{chan,MM,{error,Str}} ->
			io:format("~p~n",[Str]),
			rev();
		Other ->
			io:format("chat_client rev error!")
	end.
