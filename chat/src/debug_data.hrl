%错误处理
-define(DEBUG(X),log(X)).

log(X) ->
    io:format("~p debuf info:~p",[?MODULE,X]),
    error_logger:error_msg("~p debuf info:~p",[?MODULE,X]).
