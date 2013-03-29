%%%------------------------------------
%%% @Module  : chat_app
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/27
%%% @Description: 应用程序封装回调模块
%%%------------------------------------

-module(chat_app).
-behaviour(application).
-export([start/2,stop/1]).

start(_Type,_StartArgs) ->
    chat_supervisor:start().

stop(_State) ->
    ok.

