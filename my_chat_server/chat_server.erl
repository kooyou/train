%%%------------------------------
%%%@Module :chat_server
%%%@Author :fengzhenlin
%%%@Email  :535826356@qq.com
%%%@Created :2013.3.24
%%%@Description :聊天服务器
%%%-----------------------------

-module(chat_server).
-compile(export_all).

start() ->
	register(chat_server,spawn(fun() -> start_server() end)),
	lib_chan:start_server("chat.conf").

start_server() ->
	%生成模拟数据表user:id,psw
	UserTabId = ets:new(user,[ordered_set]),
	ets:insert(UserTabId,{100,1}),
	ets:insert(UserTabId,{101,2}),
	ets:insert(UserTabId,{102,3}),
	ets:insert(UserTabId,{103,4}),
	ets:insert(UserTabId,{104,5}),
	%ets:insert(UserTabId,{105,rose}),
	%ets:insert(UserTabId,{106,wang}),
	%ets:insert(UserTabId,{107,zhang}),
	%ets:insert(UserTabId,{108,li}),
	%ets:insert(UserTabId,{109,feng}),
	%ets:insert(UserTabId,{110,huang}),

	%数据表user_info：id,nick,login_times,chat_times,last_login
	UserinfoTabId = ets:new(user_info,[set]),
	ets:insert(UserinfoTabId,{100,joe,0,0,now()}),
	ets:insert(UserinfoTabId,{101,jane,0,0,now()}),
	ets:insert(UserinfoTabId,{102,tom,0,0,now()}),
	ets:insert(UserinfoTabId,{103,zhang,0,0,now()}),
	ets:insert(UserinfoTabId,{104,li,0,0,now()}),

	%数据表：online:id,nick
	OnlineTabId = ets:new(online,[set]),

	%PidTab = ets:new(pidtab,[set]),

	TabId = {UserTabId,UserinfoTabId,OnlineTabId},

	process_flag(trap_exit,true),
	Val=(catch server_loop(TabId)),
	io:format("Server terminated with:~p~n",[Val]).

server_loop(TabId) ->
	receive
		{chat,Channel,{login,Id,Psw}} ->
			case login_handler(Id,Psw,TabId,Channel) of
				ok -> lib_chan_mm:send(Channel,{login,ok});
				error -> lib_chan_mm:send(Channel,{login,error})
			end,
			server_loop(TabId);
		{chat,Channel,{Str,ToNick}} ->
			chat_handler(ToNick,Str,TabId,Channel),
			server_loop(TabId);
		{chat,Channel,{chat_closed,Id,Nick}} ->
			off_line_handler(Nick,TabId),
			server_loop(TabId);
		{chat_closed,_} ->
			server_loop(TabId)
	end.

chat_handler(ToNick,Str,TabId,Channel) ->
	{_UserTabId,_UserInfoTabId,OnlineTabId} = TabId ,
	case ets:lookup(OnlineTabId,ToNick) of
		[{_,ToChannel}] -> lib_chan_mm:send(ToChannel,{chat,Str});
		[] -> lib_chan_mm:send(Channel,{error,"Your friend is not online!"})
	end.
	

login_handler(Id,Psw,TabId,Channel) ->
	{UserTabId,UserInfoTabId,OnlineTabId} = TabId ,
	case ets:lookup(UserTabId,Id) of 
			[{_,Psw}] -> 
				%更新表数据
				%更新user_info表
				
				[{Uid,Unick,Ulogin_times,Uchat_times,Ulast_login}] = 
					ets:lookup(UserInfoTabId,Id),
				ets:delete(UserInfoTabId,Id),
				ets:insert(UserInfoTabId,
					{Uid,Unick,Ulogin_times+1,Uchat_times,now()}),
				%更新online表
				ets:insert(OnlineTabId,{Unick,Channel}),
				ok;
			[] -> error;
			[{_,_}] -> error
	end.

off_line_handler(Nick,TabId) -> 
	{UserTabId,UserInfoTabId,OnlineTabId} = TabId,
	ets:delete(OnlineTabId,Nick).


				

			

