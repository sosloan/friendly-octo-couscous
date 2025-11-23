%% Integration Example: Erlang calling Java via JInterface
%% Demonstrates Erlang supervisor communicating with Java

-module(java_integration).
-export([call_java_order_service/1, demo/0]).

%% Call Java Order Service
call_java_order_service(Order) ->
    %% In production, this would use JInterface or a TCP bridge
    io:format("~n=== Erlang-Java Integration ===~n"),
    io:format("Erlang sending order to Java: ~p~n", [Order]),
    
    %% Simulate Java processing
    case validate_with_java(Order) of
        {ok, Result} ->
            io:format("✓ Java validated order: ~p~n", [Result]),
            {ok, Result};
        {error, Reason} ->
            io:format("✗ Java rejected order: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Simulate Java validation (in production, this would be actual JInterface call)
validate_with_java(#{price := Price, quantity := Qty}) when Price > 0, Qty > 0 ->
    {ok, #{status => accepted, java_timestamp => erlang:system_time(millisecond)}};
validate_with_java(_) ->
    {error, invalid_order}.

%% Demo function
demo() ->
    io:format("~n╔══════════════════════════════════════╗~n"),
    io:format("║  Erlang + Java Integration Demo     ║~n"),
    io:format("╚══════════════════════════════════════╝~n"),
    
    Order = #{
        order_id => 1,
        symbol => "AAPL",
        price => 150.50,
        quantity => 100,
        side => buy
    },
    
    call_java_order_service(Order),
    
    io:format("~n🧠 Erlang supervising Java components~n"),
    io:format("💪 Fault tolerance + Performance combined~n"),
    ok.
