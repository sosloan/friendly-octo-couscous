%%%-------------------------------------------------------------------
%%% @doc HFT Match Engine Worker
%%% Core matching logic for buy and sell orders
%%% @end
%%%-------------------------------------------------------------------
-module(hft_match_engine).
-behaviour(gen_server).

-export([start_link/0, match_orders/2, get_trades/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    trades = [] :: list(),
    trade_counter = 1 :: pos_integer()
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

match_orders(BuyOrder, SellOrder) ->
    gen_server:call(?MODULE, {match, BuyOrder, SellOrder}).

get_trades() ->
    gen_server:call(?MODULE, get_trades).

%% Callbacks
init([]) ->
    io:format("⚡ Match Engine started~n"),
    {ok, #state{}}.

handle_call({match, BuyOrder, SellOrder}, _From, State) ->
    #{symbol := BuySymbol, price := BuyPrice} = BuyOrder,
    #{symbol := SellSymbol, price := SellPrice} = SellOrder,
    
    case can_match(BuySymbol, SellSymbol, BuyPrice, SellPrice) of
        true ->
            Trade = #{
                trade_id => State#state.trade_counter,
                buy_order => BuyOrder,
                sell_order => SellOrder,
                price => SellPrice,
                timestamp => erlang:system_time(millisecond)
            },
            io:format("🎯 Trade executed: ~p @ ~p~n", [BuySymbol, SellPrice]),
            NewState = State#state{
                trades = [Trade | State#state.trades],
                trade_counter = State#state.trade_counter + 1
            },
            {reply, {ok, Trade}, NewState};
        false ->
            {reply, {error, no_match}, State}
    end;

handle_call(get_trades, _From, State) ->
    {reply, lists:reverse(State#state.trades), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Match Engine terminating~n"),
    ok.

%% Internal functions
can_match(Symbol, Symbol, BuyPrice, SellPrice) when BuyPrice >= SellPrice ->
    true;
can_match(_, _, _, _) ->
    false.
