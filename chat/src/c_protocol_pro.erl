%%%------------------------------------
%%% @Module  : c_protocol_pro
%%% @Author  : fengzhenlin
%%% @Email   : fengzhelin@jieyou.cn
%%% @Created : 2013/3/27
%%% @Description: 负责客户端协议封包和解包
%%%------------------------------------

-module(c_protocol_pro).
-compile(export_all).
-include("protocol.hrl").

%%=========================================================================
%% 接口函数
%%=========================================================================


%%=========================================================================
%% 回调函数
%%=========================================================================


%获取协议命令码
get_protocol_cmd(Bin) ->
    <<MsgLen:16,Cmd:16,LeaveBin/binary>> = Bin,
    {Cmd,MsgLen}.



%根据消息命令码对消息包进行相应的处理
cmdcode_match(Cmdcode,MsgLen,Data,Is_rev) ->
    case Cmdcode of
        ?REGISTER_CMD_ID ->
            if
                Is_rev -> register_parse(MsgLen,Data);
                true -> register_pack(Data)
            end;
        ?LOGIN_CMD_ID -> 
            if
                Is_rev -> login_bag_parse(MsgLen,Data);
                true -> login_bag_pack(Data)
            end;
        ?WHOONLINE_CMD_ID -> 
            if
                Is_rev -> whoonline_bag_parse(MsgLen,Data);
                true -> whoonline_bag_pack(Data)
            end;
        ?FNDONLINE_CMD_ID -> 
            if
                Is_rev -> fndonline_bag_parse(MsgLen,Data);
                true -> fndonline_bag_pack(Data)
            end;
        ?CHAT_SEND_CMD_ID -> 
            if 
                Is_rev -> chat_send_bag_parse(MsgLen,Data);
                true -> chat_send_bag_pack(Data)
            end;
        ?CHAT_REV_CMD_ID -> 
            if
                Is_rev -> chat_rev_bag_parse(MsgLen,Data);
                true -> chat_rev_bag_pack(Data)
            end;
        ?LOGIN_TIMES_CMD_ID -> 
            if 
                Is_rev -> login_times_bag_parse(MsgLen,Data);
                true -> login_times_bag_pack(Data)
            end;
        ?CHAT_TIMES_CMD_ID -> 
            if 
                Is_rev -> chat_times_bag_parse(MsgLen,Data);
                true -> chat_times_bag_pack(Data)
            end
    end.

register_parse(_MsgLen,DataBin) ->
    <<MsgLen:?MSG_HEAD_LEN,
        CmdID:?CMD_LEN,
        Result:?INT16,
        UserID:?INT32 >> = DataBin,
        {MsgLen,CmdID,Result,UserID}.

register_pack(Data) ->
    {UserName,Psw} = Data,
    Message_Len = 16 + 16 + 16 + string:len(UserName) * 8
                        + 16 + string:len(Psw) * 8, 
    NameLen = string:len(UserName),
    PswLen = string:len(Psw),
    <<Message_Len:16,?REGISTER_CMD_ID:16,NameLen:16,
    (list_to_binary(UserName))/binary,
    PswLen:16,(list_to_binary(Psw))/binary>>.

login_bag_parse(_MsgLen,BinData) ->
    <<_RevMessage_Len:16,_Command_ID:16,Is_Succeed:16,
              _User_Id:32,StringLen:16,UserName:StringLen/binary>> = BinData,
    {Is_Succeed,UserName}.

login_bag_pack(Data) ->
    {UserId,Psw} = Data,
    %对请求消息进行协议封装
    Message_Len = 16 + 16 + 32 + 16 + string:len(Psw) * 8, 
    StrLen = string:len(Psw),
    SendBin = <<Message_Len:16,?LOGIN_CMD_ID:16,UserId:32,
    StrLen:16,(list_to_binary(Psw))/binary>>.

whoonline_bag_parse(MsgLen,Data) ->
    <<  Message_Len:16,
        Command_ID:16,
        Result:16,
        OnlineNum:32 >> = Data,
    { Message_Len,Command_ID,Result,OnlineNum}.

whoonline_bag_pack(Data) ->
    Message_Len = ?MSG_HEAD_LEN + ?CMD_LEN ,
    %封装请求协议
    SendBin = <<Message_Len:16,?WHOONLINE_CMD_ID:16>>.

fndonline_bag_parse(MsgLen,Data) ->
    void.

fndonline_bag_pack(Data) ->
    void.

chat_send_bag_parse(MsgLen,Data) ->
    void.

chat_send_bag_pack(Data) ->
    void.

chat_rev_bag_parse(MsgLen,Data) ->
    <<  Message_Len:16,
        Command_ID:16,
        Send_User_Id:32,
        NameLen:16,
        Send_User_Name:NameLen/binary,
        Send_Data_Type:16,
        DataLen:16,
        Send_Data:DataLen/binary >> = Data,

     {  Message_Len,
        Command_ID,
        Send_User_Id,
        binary_to_list(Send_User_Name),
        Send_Data_Type,
        binary_to_list(Send_Data)   }.

chat_rev_bag_pack(Data) ->
    void.

login_times_bag_parse(MsgLen,Bin) ->
    <<  MsgLen:?MSG_HEAD_LEN,
        Cmd_ID:?CMD_LEN,
        Result:?INT16,
        LoginTimes:?INT32
    >> = Bin,
    {MsgLen,Cmd_ID,Result,LoginTimes}.

login_times_bag_pack(Data) ->
    {UserId} = Data,
    Message_Len = ?MSG_HEAD_LEN + ?CMD_LEN + ?INT32,
    <<  Message_Len:?MSG_HEAD_LEN,
        ?LOGIN_TIMES_CMD_ID:?CMD_LEN,
        UserId:?INT32   >>.

chat_times_bag_parse(MsgLen,Bin) ->
    <<  MsgLen:?MSG_HEAD_LEN,
        Cmd_ID:?CMD_LEN,
        Result:?INT16,
        ChatTimes:?INT32
    >> = Bin,
    {MsgLen,Cmd_ID,Result,ChatTimes}.

chat_times_bag_pack(Data) ->
    {UserId} = Data,
    Message_Len = ?MSG_HEAD_LEN + ?CMD_LEN + ?INT32,
    <<  Message_Len:?MSG_HEAD_LEN,
        ?CHAT_TIMES_CMD_ID:?CMD_LEN,
        UserId:?INT32   >>.
