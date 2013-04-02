%%%------------------------------------
%%% @Module  : protocol_pro
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/25
%%% @Description: 负责协议的处理，如协议的封装和解包
%%%------------------------------------

-module(protocol_pro).
-compile(export_all).
-include("protocol.hrl").

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================

%%=========================================================================
%%宏
%%=========================================================================

%获取协议命令码
get_protocol_cmd(Bin) ->
    <<MsgLen:16,Cmd:16,LeaveBin/binary>> = Bin,
    {Cmd,MsgLen}.


%根据协议命令码对消息包进行分解处理
cmdcode_match(Cmdcode,MsgLen,Bin) ->
    case Cmdcode of
        ?REGISTER_CMD_ID -> register_parse(MsgLen,Bin);
        ?LOGIN_CMD_ID -> login_bag_parse(MsgLen,Bin);
        ?WHOONLINE_CMD_ID -> whoonline_bag_parse(MsgLen,Bin);
        ?FNDONLINE_CMD_ID -> fndonline_bag_parse(MsgLen,Bin);
        ?CHAT_SEND_CMD_ID -> chat_send_bag_parse(MsgLen,Bin);
        ?CHAT_REV_CMD_ID -> chat_rev_bag_parse(MsgLen,Bin);
        ?LOGIN_TIMES_CMD_ID -> login_times_bag_parse(MsgLen,Bin);
        ?CHAT_TIMES_CMD_ID -> chat_times_bag_parse(MsgLen,Bin)
    end.


%根据协议命令对消息进行封包处理
cmdcode_pack(Cmdcode,Data) ->
    case Cmdcode of
        ?REGISTER_CMD_ID -> register_pack(Data);
        ?LOGIN_CMD_ID -> login_pack(Data);
        ?CHAT_REV_CMD_ID -> chat_rev_pack(Data);
        ?WHOONLINE_CMD_ID -> whoonline_pack(Data);
        ?LOGIN_TIMES_CMD_ID -> logintimes_pack(Data);
        ?CHAT_TIMES_CMD_ID -> chattimes_pack(Data);
        ?FNDONLINE_CMD_ID -> friend_online_pack(Data);
        _Other -> void
    end.

%注册解析
register_parse(_MsgLen,Bin) ->
    <<Message_Len:16,Command_ID:16,NameLen:16,
            UserName:NameLen/binary,StrLen:16,Psw:StrLen/binary>> = Bin,
            {Message_Len,Command_ID,binary_to_list(UserName),binary_to_list(Psw)}.

%注册封包
register_pack(Data) ->
    {Result,UserID} = Data,
     MsgLen = ?MSG_HEAD_LEN + ?CMD_LEN + ?INT16 + ?INT32,
     io:format("~p,~p~n",[MsgLen,UserID]),
     <<MsgLen:?MSG_HEAD_LEN,
      ?REGISTER_CMD_ID:?CMD_LEN,
      Result:?INT16,
      UserID:?INT32>>.

%封装接收聊天信息应答包 
chat_rev_pack(Data) ->
    {UserId,UserName,Type,SendStr} = Data,
    SendStrLen = string:len(SendStr),
    UserNameLen = string:len(UserName),
    MsgLen = ?MSG_HEAD_LEN + ?CMD_LEN + ?INT32 + ?INT16 +
                UserNameLen * ?CHAR8 + ?INT16 + SendStrLen * ?CHAR8,
    SendBin = <<MsgLen:?MSG_HEAD_LEN,
                ?CHAT_REV_CMD_ID:?CMD_LEN,
                UserId:?INT32,
                UserNameLen:?INT16,
                (list_to_binary(UserName))/binary,
                Type:?INT16,
                SendStrLen:?INT16,
                (list_to_binary(SendStr))/binary>>,
    SendBin.


%解析登录请求包
login_bag_parse(MsgLen,Bin) ->
    <<Message_Len:16,Command_ID:16,
            UserId:32,StrLen:16,Psw:StrLen/binary>> = Bin,
            {Message_Len,Command_ID,UserId,binary_to_list(Psw)}.
%封装登录应答包
login_pack(Data) ->
    {Result,UserId,Name} = Data,
    StrLen = string:len(Name),
    Message_Len = 16+16+16+32+16+StrLen,
    SendBin = << Message_Len:16,?LOGIN_CMD_ID:16,Result:16,
            UserId:32,StrLen:16,(list_to_binary(Name))/binary >>.

%解析查看在线用户请求包
whoonline_bag_parse(MsgLen,Bin) ->
    <<Message_Len:?MSG_HEAD_LEN,Command_ID:?CMD_LEN>> = Bin,
    {Message_Len,Command_ID}.

%封装查看在线用户应答包
whoonline_pack(Data) ->
    Message_Len = ?MSG_HEAD_LEN + ?CMD_LEN + ?INT32,
    {Result,OnlineNum} = Data,
    <<Message_Len:?MSG_HEAD_LEN,?WHOONLINE_CMD_ID:?CMD_LEN,
            Result:?INT16,OnlineNum:?INT32>>.


%解析查看在线朋友请求包
fndonline_bag_parse(MsgLen,Bin) ->
    void.

%解析发送聊天消息请求包
chat_send_bag_parse(MsgLen,Bin) ->
    <<  Message_Len:16,
        Command_ID:16,
        Send_User_Id:32,
        NameLen:16,
        Send_User_Name:NameLen/binary,
        RevNameLen:16,
        Rev_User_Name:RevNameLen/binary,
        Send_Data_Type:16,
        DataLen:16,
        Send_Data:DataLen/binary >> = Bin,

    {   Message_Len,
        Command_ID,
        Send_User_Id,
        binary_to_list(Send_User_Name),
        binary_to_list(Rev_User_Name),
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

logintimes_pack(Data) ->
    {Result,LoginTimes} = Data,
    MsgLen = ?MSG_HEAD_LEN + ?CMD_LEN + ?INT16 + ?INT32,
    <<  MsgLen:?MSG_HEAD_LEN,
        ?LOGIN_TIMES_CMD_ID:?CMD_LEN,
        Result:?INT16,
        LoginTimes:?INT32 >>.

%解析查看聊天次数请求包
chat_times_bag_parse(_MsgLen,Bin) ->
     << Message_Len:16,
        Command_ID:16,
        User_Id:32 >> = Bin,
    {Message_Len,Command_ID,User_Id}.

chattimes_pack(Data) ->
    {Result,ChatTimes} = Data,
    MsgLen = ?MSG_HEAD_LEN + ?CMD_LEN + ?INT16 + ?INT32,
    <<  MsgLen:?MSG_HEAD_LEN,
        ?CHAT_TIMES_CMD_ID:?CMD_LEN,
        Result:?INT16,
        ChatTimes:?INT32 >>.

friend_online_pack(Data) ->
    {Result,Array} = Data,
    N = length(Array),
    F = fun(Name) ->
            StrLen = string:len(Name),
            <<StrLen:?INT16,Name/binary>>
    end,
    LB = list_to_binary([F(X) || X <- Array]),
    <<N:?INT16,LB/binary>>.

