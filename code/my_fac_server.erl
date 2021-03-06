%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(my_fac_server).
-export([loop/0]).

loop() ->
    receive
	{From, {fac, N}} ->
	    From ! {self(), fac(N)},
	    loop();
	{become, Something} ->
	    Something()
    end.
    
fac(0) -> 1;
fac(N) -> N * fac(N-1).
