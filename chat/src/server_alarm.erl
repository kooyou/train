%%%------------------------------------
%%% @Module  : server_alarm
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/27
%%% @Description: 聊天服务器警报管理
%%%------------------------------------

-module(server_alarm).
-behaviour(gen_event).

-include("debug_data.hrl").

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================
-export([init/1,handle_event/2,handle_call/2,
          handle_info/2,terminate/2]).

init(Args) ->
    {ok,0}.

handle_event({set_alarm,tooMuchClient},N) ->
    error_logger:error_msg("***Too much client connected.~n"),
    {ok,N+1};

handle_event({clear_alarm,tooMuchClient},N) ->
    error_logger:error_msg("***Too much client alarm cancle.~n"),
    {ok,N};

handle_event(Event,N) ->
    io:format("***unmatched event:~p~n",[Event]),
    {ok,N}.

handle_call(_Request,N) -> Reply = N,{ok,N,N}.

handle_info(_Info,N) -> {ok,N}.

terminate(_Reason,_N) -> ok.


