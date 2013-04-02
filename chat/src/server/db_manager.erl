%%%------------------------------------
%%% @Module  : db_manager
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/28
%%% @Description: 管理mysql数据库
%%%------------------------------------

-module(db_manager).
-compile(export_all).

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

-define(CHAT_DB,chat_db).
init_db() ->
     %打开数据库test
    mysql:start_link(?CHAT_DB,"localhost","root","123","test").

get_user_psw(UserID) ->
     SqlStr = io_lib:format("select psw from user_key where id = ~p;",[UserID]),
     {data,Res} = mysql:fetch(?CHAT_DB,SqlStr),
     AllRows = mysql:get_result_rows(Res),
     AllRows.

get_user_f_name(UserName) ->
    SqlStr = io_lib:format("select id from user_info where name = '~s';",[UserName]),
    {data,Res} = mysql:fetch(?CHAT_DB,SqlStr),
    AllRows = mysql:get_result_rows(Res),
    AllRows.

add_user(UserId,UserName,Psw) ->
    SqlStr = io_lib:format("insert into user_key values(~p,'~s')",[UserId,Psw]),
    mysql:fetch(?CHAT_DB,SqlStr),
    SqlStr1 = io_lib:format("insert into user_info values(~p,'~s',0,0,0)",[UserId,UserName]),
    mysql:fetch(?CHAT_DB,SqlStr1).

get_new_id() ->
    {data,Res} = mysql:fetch(?CHAT_DB,"select max(id) from user_key"),
    AllRows = mysql:get_result_rows(Res),
    AllRows.

get_user_info(UserId) ->
    SqlStr = io_lib:format("select * from user_info where id = ~p",[UserId]),
    {data,Res} = mysql:fetch(?CHAT_DB,SqlStr),
    AllRows = mysql:get_result_rows(Res),
    AllRows.

update_userinfo(Data) ->
    {UserId,Name,LoginTimes,ChatTimes,LastTime} = Data,
    {{YY,MM,DD},{Hh,Mm,Se}} = LastTime,
    SqlStr = io_lib:format("update user_info set login_times = ~p,chat_times = ~p, last_login = '~p-~p-~p ~p:~p:~p' where id = ~p ",[LoginTimes,ChatTimes,YY,MM,DD,Hh,Mm,Se,UserId]),
    mysql:fetch(?CHAT_DB,SqlStr).
