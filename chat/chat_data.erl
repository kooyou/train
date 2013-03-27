%%%------------------------------------
%%% @Module  : chat_data
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/26
%%% @Description: 负责对聊天系统后台数据的管理
%%%------------------------------------

-module(chat_data).
-compile(export_all).

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

data_init(Parend) ->
    %user表:user_id,psw
    UserTab = ets:new(user,[set]),

    ets:insert(UserTab,{100,"0"}),
    ets:insert(UserTab,{101,"1"}),
    ets:insert(UserTab,{102,"2"}),
    ets:insert(UserTab,{103,"3"}),

    %userInfo表:user_id,name,login_times,chat_times,last_login
    UserInfo = ets:new(userInfo,[set]),
    ets:insert(UserInfo,{100,"joe",0,0,0}),
    ets:insert(UserInfo,{101,"jane",0,0,0}),
    ets:insert(UserInfo,{102,"tom",0,0,0}),
    ets:insert(UserInfo,{103,"zhang",0,0,0}),

    %online表：name,socket
    Online = ets:new(online,[set]),

    TabId = {UserTab,UserInfo,Online},

    %通知父进程数据初始化完成
    Parend ! {datainit,ok},

    loop(TabId).


loop(TabId) ->
    receive
        {get_user_psw,UserId,Pid} ->
            Pid ! get_user_psw(TabId,UserId);
        {get_user_name,UserId,Pid} ->
            Pid ! get_user_name(TabId,UserId);
        {get_online_name,Socket,Pid} ->
            Pid ! get_online_name(TabId,Socket);
        {add_online,Socket,UserId,UserName} ->
            add_online(TabId,Socket,UserId,UserName);
        {del_online,Socket} ->
            del_online(TabId,Socket);
        {add_login_times,UserId} ->
            add_login_times(TabId,UserId);
        {get_login_times,UserId,Pid} ->
            Pid ! {login_times,get_login_times(TabId,UserId)};
        {add_chat_times,UserId} ->
            add_chat_times(TabId,UserId);
        {get_chat_times,UserId,Pid} ->
            Pid ! {chat_times,get_chat_times(TabId,UserId)};
        {update_lastlogin,UserId} ->
            update_lastlogin(TabId,UserId);
        {get_online_num,Pid} ->
            Pid ! {online,get_online_num(TabId)};
        Other ->
            io:format("chat_data:error data require:~p~n",[Other])            
    end,
    loop(TabId).

%查看登录次数
get_login_times(TabId,UserId) ->
    {_UserTab,UserInfo,_Online} = TabId,
    case ets:lookup(UserInfo,UserId) of
        [{_,_,LoginTimes,_,_}] -> LoginTimes;
        _Other -> 0
    end.

%查看聊天次数
get_chat_times(TabId,UserId)->
    {_UserTab,UserInfo,_Online} = TabId,
    case ets:lookup(UserInfo,UserId) of
        [{_,_,_,ChatTimes,_}] -> ChatTimes;
        _Other -> 0
    end.

%查看在线人数
get_online_num(TabId) ->
    {_UserTab,_UserInfo,Online} = TabId,
    ets:info(Online,size).

%更新用户最后一次登录时间
update_lastlogin(TabId,UserId) ->
    {_UserTab,UserInfo,_Online} = TabId,
    [{UserId,UserName,Logintimes,ChatTimes,_LastLogin}] = 
                    ets:lookup(UserInfo,UserId),
    NewState = {UserId,UserName,Logintimes,ChatTimes,
        timer_handler:now_to_local_string(now())},
    ets:insert(UserInfo,NewState).


%增加用户聊天次数
add_chat_times(TabId,UserId) ->
    {_UserTab,UserInfo,_Online} = TabId,
    [{UserId,UserName,Logintimes,ChatTimes,LastLogin}] = 
                    ets:lookup(UserInfo,UserId),
    NewState = {UserId,UserName,Logintimes,ChatTimes+1,LastLogin},
    ets:insert(UserInfo,NewState).

%增加用户登录次数
add_login_times(TabId,UserId) ->
    {_UserTab,UserInfo,_Online} = TabId,
    [{UserId,UserName,Logintimes,ChatTimes,LastLogin}] = 
                    ets:lookup(UserInfo,UserId),
    NewState = {UserId,UserName,Logintimes+1,ChatTimes,LastLogin},
    ets:insert(UserInfo,NewState).


%根据用户ID获取用户登录密码
get_user_psw(TabId,UserId) ->
    {UserTab,_UserInfo,_Online} = TabId,
    ets:lookup(UserTab,UserId).

%根据用户ID获取用户名
get_user_name(TabId,UserId) ->
    {_UserTab,UserInfo,_Online} = TabId,
    ets:lookup(UserInfo,UserId).

%根据客户端Socket获取用户名
get_online_name(TabId,Socket) ->
    {_UserTab,_UserInfo,Online} = TabId,
    ets:lookup(Online,Socket).

%添加在线用户
add_online(TabId,Socket,UserId,UserName) ->
    {_UserTab,_UserInfo,Online} = TabId,
    ets:insert(Online,{Socket,UserId,UserName}).

%删除在线用户
del_online(TabId,Socket) ->
    {_UserTab,_UserInfo,Online} = TabId,
    ets:delete(Online,Socket).

