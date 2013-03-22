-module(find_odd).
-export([find_odd/1]).

find_odd(L) -> { [Odd || Odd <- L,Odd rem 2 =/= 0] }.
