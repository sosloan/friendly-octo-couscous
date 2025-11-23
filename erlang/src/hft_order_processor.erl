%%%-------------------------------------------------------------------
%%% @doc HFT Order Processor Worker
%%% Handles order submission and validation
%%% @end
%%%-------------------------------------------------------------------
-module(hft_order_processor).
-behaviour(gen_server).

-export([start_link/0, submit_order/1, get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    orders_processed = 0 :: non_neg_integer(),
    orders_rejected = 0 :: non_neg_integer()
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

submit_order(Order) ->
    gen_server:call(?MODULE, {submit_order, Order}).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% Callbacks
init([]) ->
    io:format("📊 Order Processor started~n"),
    {ok, #state{}}.

handle_call({submit_order, Order}, _From, State) ->
    case validate_order(Order) of
        true ->
            io:format("✓ Order accepted: ~p~n", [Order]),
            {reply, {ok, accepted}, State#state{orders_processed = State#state.orders_processed + 1}};
        false ->
            io:format("✗ Order rejected: ~p~n", [Order]),
            {reply, {error, invalid}, State#state{orders_rejected = State#state.orders_rejected + 1}}
    end;

handle_call(get_stats, _From, State) ->
    Stats = #{
        processed => State#state.orders_processed,
        rejected => State#state.orders_rejected
    },
    {reply, Stats, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Order Processor terminating~n"),
    ok.

%% Internal functions
validate_order(#{price := Price, quantity := Qty}) when Price > 0, Qty > 0 ->
    true;
validate_order(_) ->
    false.
