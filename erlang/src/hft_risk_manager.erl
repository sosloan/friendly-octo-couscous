%%%-------------------------------------------------------------------
%%% @doc HFT Risk Manager Worker
%%% Monitors and enforces risk limits
%%% @end
%%%-------------------------------------------------------------------
-module(hft_risk_manager).
-behaviour(gen_server).

-export([start_link/0, check_risk/1, get_exposure/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    total_exposure = 0.0 :: float(),
    max_exposure = 1000000.0 :: float(),
    alerts = [] :: list()
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_risk(Order) ->
    gen_server:call(?MODULE, {check_risk, Order}).

get_exposure() ->
    gen_server:call(?MODULE, get_exposure).

%% Callbacks
init([]) ->
    io:format("🛡️  Risk Manager started~n"),
    {ok, #state{}}.

handle_call({check_risk, Order}, _From, State) ->
    #{price := Price, quantity := Qty} = Order,
    OrderValue = Price * Qty,
    NewExposure = State#state.total_exposure + OrderValue,
    
    case NewExposure =< State#state.max_exposure of
        true ->
            io:format("✓ Risk check passed: Exposure ~p~n", [NewExposure]),
            {reply, {ok, approved}, State#state{total_exposure = NewExposure}};
        false ->
            Alert = #{
                timestamp => erlang:system_time(millisecond),
                reason => exposure_exceeded,
                current => NewExposure,
                limit => State#state.max_exposure
            },
            io:format("⚠️  Risk check failed: Exposure limit exceeded~n"),
            {reply, {error, risk_limit}, State#state{alerts = [Alert | State#state.alerts]}}
    end;

handle_call(get_exposure, _From, State) ->
    Exposure = #{
        current => State#state.total_exposure,
        max => State#state.max_exposure,
        alerts => length(State#state.alerts)
    },
    {reply, Exposure, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Risk Manager terminating~n"),
    ok.
