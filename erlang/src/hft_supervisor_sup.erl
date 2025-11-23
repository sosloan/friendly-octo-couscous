%%%-------------------------------------------------------------------
%%% @doc HFT Top-Level Supervisor
%%% Implements one_for_one strategy - if a child dies, only restart that child
%%% @end
%%%-------------------------------------------------------------------
-module(hft_supervisor_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %% Supervisor flags
    SupFlags = #{
        strategy => one_for_one,  % Restart only failed child
        intensity => 10,          % Max 10 restarts
        period => 60              % Within 60 seconds
    },
    
    %% Child specifications
    ChildSpecs = [
        #{
            id => hft_order_processor,
            start => {hft_order_processor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [hft_order_processor]
        },
        #{
            id => hft_match_engine,
            start => {hft_match_engine, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [hft_match_engine]
        },
        #{
            id => hft_risk_manager,
            start => {hft_risk_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [hft_risk_manager]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
