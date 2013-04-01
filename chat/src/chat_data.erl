%%%------------------------------------
%%% @Module  : chat_data
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/26
%%% @Description: 负责对聊天系统后台数据的管理
%%%------------------------------------

-module(chat_data).
-compile(export_all).
%-export([start/0,start_link/0]).
%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================
%-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
%         terminate/2,code_change/3]).


start() ->
    io:format("chat_data starting....~n"),
    register(?MODULE,spawn_link(fun() -> data_init(server) end)),
    {ok, whereis(?MODULE)}.

data_init(Parend) ->
    %初始化mysql数据
    db_manager:init_db(),

    %user表:user_id,psw
    UserTab = ets:new(user,[set]),

    %userInfo表:user_id,name,login_times,chat_times,last_login
    UserInfo = ets:new(userInfo,[set]),

    %online表：socket,id,name
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
        {add_user_info,UserId} ->
            add_user_info(TabId,UserId);
        {del_user_info,Socket} ->
            del_user_info(TabId,Socket);
        {all_online_socket,Pid} ->
            Pid ! {all_online,get_all_online_socket(TabId)};
        Other ->
            io:format("chat_data:error data require:~p~n",[Other])            
    end,
    loop(TabId).

%添加在线用户信息
add_user_info(TabId,UserId)->
    Data = get_user_info(UserId),
    {_UserTab,UserInfo,_Online} = TabId,
    ets:insert(UserInfo,Data).

%删除在线用户信息
del_user_info(TabId,Socket) ->
    {_UserTab,UserInfo,Online} = TabId,
    [{_,UserId,_}] = ets:lookup(Online,Socket),
    [Data] = ets:lookup(UserInfo,UserId),
    db_manager:update_userinfo(Data).


    

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
    %{UserTab,_UserInfo,_Online} = TabId,
    %ets:lookup(UserTab,UserId).
    Pswtem = db_manager:get_user_psw(UserId),
    [Psw1] = Pswtem,
    [Psw2] = Psw1,
    Psw = binary_to_list(Psw2),
    io:format("chat_data:~p,~p~n",[UserId,Psw]),
    [{UserId,Psw}].

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

%遍历online表的socket
get_all_online_socket(TabId) ->
    {_UserTab,_UserInfo,Online} = TabId,
    List = get_all(Online,first,[],[]).

get_all(TabId,Cmd,L,Key1) ->
    case Cmd of
        first -> 
            case ets:first(TabId) of
                '$end_of_table' -> L;
                Value -> get_all(TabId,next,[Value|L],Value)
            end;
        next ->
            case ets:next(TabId,Key1) of
                '$end_of_table' -> L;
                Value -> get_all(TabId,next,[Value|L],Value)
            end
    end.



%%%%%%%%操作mysql%%%%%%%%%%%%%%%%%%%
get_user_f_name(UserName) ->
    db_manager:get_user_f_name(UserName).

add_user(UserId,UserName,Psw) ->
    db_manager:add_user(UserId,UserName,Psw).

get_user_info(UserId) ->
    Result = db_manager:get_user_info(UserId),
    [[Id,Name,LoginTimes,ChatTimes,{time,{Hour,Min,Sec}}]] = Result,
    
    io:format("~p,~p~n",[Result,UserId]),
    {Id,binary_to_list(Name),LoginTimes,ChatTimes,0}.

get_new_id() ->
    NewId = db_manager:get_new_id(),
    [Tem] = NewId,
    [ID] = Tem,
    ID+1.
