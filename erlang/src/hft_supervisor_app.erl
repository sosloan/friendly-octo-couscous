%%%-------------------------------------------------------------------
%%% @doc HFT Supervisor Application
%%% The Immortal Supervisor - Ensures system never dies
%%% @end
%%%-------------------------------------------------------------------
-module(hft_supervisor_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("~n=== Erlang/OTP Immortal Supervisor 🧠 ===~n"),
    io:format("🔄 Starting fault-tolerant supervision tree~n"),
    io:format("⚡ OTP behaviors activated~n"),
    
    case hft_supervisor_sup:start_link() of
        {ok, Pid} ->
            io:format("✓ Supervisor started successfully~n"),
            io:format("🛡️  System protected with automatic restarts~n"),
            {ok, Pid};
        Error ->
            io:format("✗ Failed to start supervisor: ~p~n", [Error]),
            Error
    end.

stop(_State) ->
    io:format("~n=== Supervisor Shutdown ===~n"),
    ok.
