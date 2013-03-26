%%%------------------------------------
%%% @Module  : protocol_pro
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/23
%%% @Description: 负责协议的处理，如协议的封装和解包
%%%------------------------------------

-module(protocol_pro).
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
-define(LOGING_CMD_ID,10001).         %登录请求命令码
-define(WHOONLINE_CMD_ID,10002).      %查看在线请求命令码
-define(FNDONLINE_CMD_ID,10003).      %查看在线朋友请求命令码  
-define(CHAT_SEND_CMD_ID,10004).      %发送聊天信息请求命令码
-define(CHAT_REV_CMD_ID,10005).       %接收聊天信息请求命令码
-define(LOGIN_TIMES_CMD_ID,10006).    %查看登录次数请求命令码
-define(CHAT_TIMES_CMD_ID,10007).     %查看聊天次数请求命令码

%获取协议命令码
get_protocol_cmd(Bin) ->
    <<MsgLen:16,Cmd:16,LeaveBin/binary>> = Bin,
    {Cmd,MsgLen}.


%根据消息命令码对消息包进行相应的分解
cmdcode_match(Cmdcode,MsgLen,Bin) ->
    case Cmdcode of
        ?LOGING_CMD_ID -> login_bag_parse(MsgLen,Bin);
        ?WHOONLINE_CMD_ID -> whoonline_bag_parse(MsgLen,Bin);
        ?FNDONLINE_CMD_ID -> fndonline_bag_parse(MsgLen,Bin);
        ?CHAT_SEND_CMD_ID -> chat_send_bag_parse(MsgLen,Bin);
        ?CHAT_REV_CMD_ID -> chat_rev_bag_parse(MsgLen,Bin);
        ?LOGIN_TIMES_CMD_ID -> login_times_bag_parse(MsgLen,Bin);
        ?CHAT_TIMES_CMD_ID -> chat_times_bag_parse(MsgLen,Bin)
    end.

%解析登录请求包
login_bag_parse(MsgLen,Bin) ->
    <<Message_Len:16,Command_ID:16,
            UserId:32,StrLen:16,Psw:StrLen/binary>> = Bin,
    {Message_Len,Command_ID,UserId,Psw}.

%解析查看在线用户请求包
whoonline_bag_parse(MsgLen,Bin) ->
    void.

%解析查看在线朋友请求包
fndonline_bag_parse(MsgLen,Bin) ->
    void.

%解析发送消息请求包
chat_send_bag_parse(MsgLen,Bin) ->
    <<  Message_Len:16,
        Command_ID:16,
        Send_User_Id:32,
        NameLen:16,
        Send_User_Name:NameLen/binary,
        Receive_User_Id:32,
        Send_Data_Type:16,
        DataLen:16,
        Send_Data:DataLen/binary >> = Bin,

    {   Message_Len,
        Command_ID,
        Send_User_Id,
        binary_to_list(Send_User_Name),
        Receive_User_Id,
        Send_Data_Type,
        binary_to_list(Send_Data)   }.

%解析接收消息应答包
chat_rev_bag_parse(MsgLen,Bin) ->
    void.

%解析查看登录次数请求包
login_times_bag_parse(MsgLen,Bin) ->
    <<  Message_Len:16,
        Command_ID:16,
        User_Id:32 >> = Bin,
    {Message_Len,Command_ID,User_Id}.

%解析查看聊天次数请求包
chat_times_bag_parse(MsgLen,Bin) ->
    void.
