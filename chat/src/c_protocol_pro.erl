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
    <<MsgLen:?INT16,Cmd:?INT16,LeaveBin/binary>> = Bin,
    {Cmd,MsgLen}.


%根据消息命令码对消息包进行相应的处理
cmdcode_match(Cmdcode,MsgLen,Data,Is_rev) ->
    if 
        Cmdcode > 10000 andalso Cmdcode =< 10200 ->
             case Is_rev of
                 true -> protocol_parse(Cmdcode,Data);
                 false -> protocol_pack(Cmdcode,Data)
             end;
         true -> io:format("cmdcode_match: bad cmdcode")
     end.

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%协议包解析%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%解析 注册 协议包
protocol_parse(?REGISTER_CMD_ID,DataBin) ->
    <<MsgLen:?MSG_HEAD_LEN,
        CmdID:?CMD_LEN,
        Result:?INT16,
        UserID:?INT32 >> = DataBin,
        {MsgLen,CmdID,Result,UserID};


%解析 登录 协议包
protocol_parse(?LOGIN_CMD_ID,BinData) ->
    <<_RevMessage_Len:?INT16,_Command_ID:?INT16,Is_Succeed:?INT16,
              _User_Id:?INT32,StringLen:?INT16,UserName:StringLen/binary>> = BinData,
    {Is_Succeed,UserName};

%解析 查看在线人数 协议包
protocol_parse(?WHOONLINE_CMD_ID,Data) ->
    <<  Message_Len:?INT16,
        Command_ID:?INT16,
        Result:?INT16,
        OnlineNum:?INT32 >> = Data,
    { Message_Len,Command_ID,Result,OnlineNum};


%解析 朋友在线 协议包 -----还没实现
protocol_parse(?FNDONLINE_CMD_ID,Data) ->
    void;

%解析 接收信息 协议包
protocol_parse(?CHAT_REV_CMD_ID,Data) ->
    <<  Message_Len:?INT16,
        Command_ID:?INT16,
        Send_User_Id:?INT32,
        NameLen:?INT16,
        Send_User_Name:NameLen/binary,
        Send_Data_Type:?INT16,
        DataLen:?INT16,
        Send_Data:DataLen/binary >> = Data,

     {  Message_Len,
        Command_ID,
        Send_User_Id,
        binary_to_list(Send_User_Name),
        Send_Data_Type,
        binary_to_list(Send_Data)   };


%解析 查看登录次数 协议包
protocol_parse(?LOGIN_TIMES_CMD_ID,Bin) ->
    <<  MsgLen:?MSG_HEAD_LEN,
        Cmd_ID:?CMD_LEN,
        Result:?INT16,
        LoginTimes:?INT32
    >> = Bin,
    {MsgLen,Cmd_ID,Result,LoginTimes};


%解析 查看聊天次数 协议包
protocol_parse(?CHAT_TIMES_CMD_ID,Bin) ->
    <<  MsgLen:?MSG_HEAD_LEN,
        Cmd_ID:?CMD_LEN,
        Result:?INT16,
        ChatTimes:?INT32
    >> = Bin,
    {MsgLen,Cmd_ID,Result,ChatTimes};

%错误的协议包解析命令
protocol_parse(_CMD_ID,_Data) ->
    io:format("protocol_parse:error cmd id!").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%协议包封装%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%注册
protocol_pack(?REGISTER_CMD_ID,Data) ->
    {UserName,Psw} = Data,
    Message_Len = ?INT16 + ?INT16 + ?INT16 + string:len(UserName) * ?CHAR8
                        + ?INT16 + string:len(Psw) * ?CHAR8, 
    NameLen = string:len(UserName),
    PswLen = string:len(Psw),
    <<Message_Len:?INT16,?REGISTER_CMD_ID:?INT16,NameLen:?INT16,
    (list_to_binary(UserName))/binary,
    PswLen:?INT16,(list_to_binary(Psw))/binary>>;

%登录
protocol_pack(?LOGIN_CMD_ID,Data) ->
    {UserId,Psw} = Data,
    %对请求消息进行协议封装
    Message_Len = ?INT16 + ?INT16 + ?INT32 + ?INT16 + string:len(Psw) * ?CHAR8, 
    StrLen = string:len(Psw),
    SendBin = <<Message_Len:?INT16,?LOGIN_CMD_ID:?INT16,UserId:?INT32,
    StrLen:?INT16,(list_to_binary(Psw))/binary>>;

%查看在线人数
protocol_pack(?WHOONLINE_CMD_ID,Data) ->
    Message_Len = ?MSG_HEAD_LEN + ?CMD_LEN ,
    %封装请求协议
    SendBin = <<Message_Len:?INT16,?WHOONLINE_CMD_ID:?INT16>>;


%查看在线朋友
protocol_pack(?FNDONLINE_CMD_ID,Data) ->
    void;

%聊天发送
protocol_pack(?CHAT_SEND_CMD_ID,Data) ->
    {UserId,UserName,RevUID,Type,SendData} = Data,
    NameLen = string:len(UserName),
    StrLen = string:len(SendData),
    Message_Len = ?MSG_HEAD_LEN + ?CMD_LEN + ?INT32 + ?INT16 +
                    NameLen * ?CHAR8 + ?INT32 + ?INT16 + StrLen * ?CHAR8,

    SendBin = <<Message_Len:?INT16,             %消息长度
                ?CHAT_SEND_CMD_ID:?INT16,       %消息命令码
                UserId:?INT32,      %发送信息的用户ID
                NameLen:?INT16,                 %用户名称长度
                                            %发送信息的用户名称
                (list_to_binary(UserName))/binary,
                RevUID:?INT32,                       %接收信息的用户ID
                0:?INT16,                       %消息数据类型string
                StrLen:?INT16,                  %信息体长度
                                            %信息体
                (list_to_binary(SendData))/binary>>;

%查看登录次数
protocol_pack(?LOGIN_TIMES_CMD_ID,Data) ->
    {UserId} = Data,
    Message_Len = ?MSG_HEAD_LEN + ?CMD_LEN + ?INT32,
    <<  Message_Len:?MSG_HEAD_LEN,
        ?LOGIN_TIMES_CMD_ID:?CMD_LEN,
        UserId:?INT32   >>;

%查看聊天次数
protocol_pack(?CHAT_TIMES_CMD_ID,Data) ->
    {UserId} = Data,
    Message_Len = ?MSG_HEAD_LEN + ?CMD_LEN + ?INT32,
    <<  Message_Len:?MSG_HEAD_LEN,
        ?CHAT_TIMES_CMD_ID:?CMD_LEN,
        UserId:?INT32   >>;


%错误命令
protocol_pack(_CMD_ID,_Data) ->
    io:format("protocol_pack:error cmd id!").
