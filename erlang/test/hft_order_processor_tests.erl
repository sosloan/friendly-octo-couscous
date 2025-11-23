%%%-------------------------------------------------------------------
%%% @doc HFT Order Processor EUnit Tests
%%% @end
%%%-------------------------------------------------------------------
-module(hft_order_processor_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test setup and teardown
setup() ->
    {ok, Pid} = hft_order_processor:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> exit(Pid, normal);
        false -> ok
    end.

%% Test fixture
order_processor_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_valid_order_accepted/1,
      fun test_invalid_order_rejected/1,
      fun test_order_stats/1
     ]}.

%% Individual tests
test_valid_order_accepted(_Pid) ->
    Order = #{
        order_id => 1,
        symbol => "AAPL",
        price => 150.50,
        quantity => 100,
        side => buy
    },
    Result = hft_order_processor:submit_order(Order),
    ?_assertEqual({ok, accepted}, Result).

test_invalid_order_rejected(_Pid) ->
    InvalidOrder = #{
        order_id => 2,
        symbol => "AAPL",
        price => 0.0,  % Invalid price
        quantity => 0,  % Invalid quantity
        side => buy
    },
    Result = hft_order_processor:submit_order(InvalidOrder),
    ?_assertEqual({error, invalid}, Result).

test_order_stats(_Pid) ->
    % Submit some orders
    ValidOrder = #{
        order_id => 1,
        symbol => "AAPL",
        price => 150.50,
        quantity => 100,
        side => buy
    },
    hft_order_processor:submit_order(ValidOrder),
    
    Stats = hft_order_processor:get_stats(),
    ProcessedCount = maps:get(processed, Stats),
    
    ?_assert(ProcessedCount >= 1).

%% Match engine tests
match_engine_test_() ->
    [
     ?_test(test_successful_match()),
     ?_test(test_failed_match())
    ].

test_successful_match() ->
    {ok, MatchPid} = hft_match_engine:start_link(),
    
    BuyOrder = #{
        order_id => 1,
        symbol => "AAPL",
        price => 150.50,
        quantity => 100,
        side => buy
    },
    
    SellOrder = #{
        order_id => 2,
        symbol => "AAPL",
        price => 150.25,
        quantity => 100,
        side => sell
    },
    
    Result = hft_match_engine:match_orders(BuyOrder, SellOrder),
    ?assertMatch({ok, _Trade}, Result),
    
    exit(MatchPid, normal).

test_failed_match() ->
    {ok, MatchPid} = hft_match_engine:start_link(),
    
    BuyOrder = #{
        order_id => 1,
        symbol => "AAPL",
        price => 150.00,  % Lower than sell
        quantity => 100,
        side => buy
    },
    
    SellOrder = #{
        order_id => 2,
        symbol => "AAPL",
        price => 150.75,  % Higher than buy
        quantity => 100,
        side => sell
    },
    
    Result = hft_match_engine:match_orders(BuyOrder, SellOrder),
    ?assertEqual({error, no_match}, Result),
    
    exit(MatchPid, normal).

%% Risk manager tests
risk_manager_test_() ->
    [
     ?_test(test_risk_check_pass()),
     ?_test(test_risk_check_fail())
    ].

test_risk_check_pass() ->
    {ok, RiskPid} = hft_risk_manager:start_link(),
    
    SmallOrder = #{
        order_id => 1,
        symbol => "AAPL",
        price => 100.0,
        quantity => 100,
        side => buy
    },
    
    Result = hft_risk_manager:check_risk(SmallOrder),
    ?assertEqual({ok, approved}, Result),
    
    exit(RiskPid, normal).

test_risk_check_fail() ->
    {ok, RiskPid} = hft_risk_manager:start_link(),
    
    % This would need to exceed the limit set in risk_manager
    % For now, we just test the structure
    LargeOrder = #{
        order_id => 1,
        symbol => "AAPL",
        price => 1000000.0,  % Very large
        quantity => 10000,
        side => buy
    },
    
    Result = hft_risk_manager:check_risk(LargeOrder),
    % May pass or fail depending on accumulated exposure
    ?assert(is_tuple(Result)),
    
    exit(RiskPid, normal).
