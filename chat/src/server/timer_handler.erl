-module(timer_handler).
-compile(export_all).


timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
      io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second])).


now_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
    calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
            [Year, Month, Day, Hour, Minute, Second, MicroSecs])).

now_to_local_string({MegaSecs, Secs, MicroSecs}) ->
    LocalTime = calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
    UTCTime = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    Seconds = calendar:datetime_to_gregorian_seconds(LocalTime) -
            calendar:datetime_to_gregorian_seconds(UTCTime),
    {{H, M, _}, Sign} = if
                Seconds < 0 ->
                {calendar:seconds_to_time(-Seconds), "-"};
                true ->
                {calendar:seconds_to_time(Seconds), "+"}
    end,
    {{Year, Month, Day}, {Hour, Minute, Second}} = LocalTime,
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0w~s~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second, MicroSecs, Sign, H, M])).


% yyyy-mm-ddThh:mm:ss[.sss]{Z|{+|-}hh:mm} -> {MegaSecs, Secs, MicroSecs}
datetime_string_to_timestamp(TimeStr) ->
    case catch parse_datetime(TimeStr) of
    {'EXIT', _Err} ->
        undefined;
    TimeStamp ->
        TimeStamp
    end.

parse_datetime(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, "T"),
    D = parse_date(Date),
    {T, MS, TZH, TZM} = parse_time(Time),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = (S - S1) - TZH * 60 * 60 - TZM * 60,
    {Seconds div 1000000, Seconds rem 1000000, MS}.

% yyyy-mm-dd
parse_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    case calendar:valid_date(Date1) of
    true ->
        Date1;
    _ ->
        false
    end.

% hh:mm:ss[.sss]TZD
parse_time(Time) ->
    case string:str(Time, "Z") of
    0 ->
        parse_time_with_timezone(Time);
    _ ->
        [T | _] = string:tokens(Time, "Z"),
        {TT, MS} = parse_time1(T),
        {TT, MS, 0, 0}
    end.

parse_time_with_timezone(Time) ->
    case string:str(Time, "+") of
    0 ->
        case string:str(Time, "-") of
        0 ->
            false;
        _ ->
            parse_time_with_timezone(Time, "-")
        end;
    _ ->
        parse_time_with_timezone(Time, "+")
    end.

parse_time_with_timezone(Time, Delim) ->
    [T, TZ] = string:tokens(Time, Delim),
    {TZH, TZM} = parse_timezone(TZ),
    {TT, MS} = parse_time1(T),
    case Delim of
    "-" ->
        {TT, MS, -TZH, -TZM};
    "+" ->
        {TT, MS, TZH, TZM}
    end.

parse_timezone(TZ) ->
    [H, M] = string:tokens(TZ, ":"),
    {[H1, M1], true} = check_list([{H, 12}, {M, 60}]),
    {H1, M1}.

parse_time1(Time) ->
    [HMS | T] =  string:tokens(Time, "."),
    MS = case T of
         [] ->
         0;
         [Val] ->
         list_to_integer(string:left(Val, 6, $0))
     end,
    [H, M, S] = string:tokens(HMS, ":"),
    {[H1, M1, S1], true} = check_list([{H, 24}, {M, 60}, {S, 60}]),
    {{H1, M1, S1}, MS}.

check_list(List) ->
    lists:mapfoldl(
      fun({L, N}, B)->
      V = list_to_integer(L),
      if
          (V >= 0) and (V =< N) ->
          {V, B};
          true ->
          {false, false}
      end
      end, true, List).

