-module(bb_stream).

-behaviour(gen_server).

-export([start_link/1]).
-export([stop/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("bb.hrl").

-record(state, {
    pid          :: undefined | pid(),
    symbol       :: undefined | string(),
    quantity = 0 :: float(),
    profit = 0.0 :: float(),
    remain = 0.0 :: float()
}).

-type opts() :: #{
    symbol   := string(),
    quantity := float(),
    profit   := float()
}.

-export_type([opts/0]).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

stop() ->
    gen_server:call(?MODULE, stop).

init(Opts) ->
    #{
        symbol   := Symbol,
        quantity := Quantity,
        profit   := Profit
    } = Opts,
    URL = bb:get_env(stream_url),
    Port = bb:get_env(stream_port),
    GunOptions = #{
        protocols     => [http],
        transport     => ssl,
        retry_timeout => 1000,
        http_opts     => #{keepalive => 5000}
    },
    {ok, Pid} = gun:open(URL, Port, GunOptions),
    State = #state{
        pid      = Pid,
        symbol   = string:to_lower(Symbol),
        profit   = Profit,
        quantity = Quantity,
        remain   = maps:get(remain, Opts, 0.0)
    },
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info({gun_up, _Pid, http}, State) ->
    ?INFO("Connection to stream server is established"),
    {ok, _RestPid} = bb_rest:start_link(),
    {ok, _TradePid} = bb_trade:start_link(),
    gun:ws_upgrade(State#state.pid, ticker_path(State)),
    {noreply, State};

handle_info({gun_ws, _Pid, {text, Data}}, State) ->
    #state{
        symbol   = Symbol,
        quantity = Quantity,
        profit   = Profit,
        remain   = Remain
    } = State,
    #{
        b := B, % BID
        a := A  % ASK
    } = bb:from_json(Data),
    BID = binary_to_float(B),
    ASK = binary_to_float(A),

    BestSellPrice = best_sell_price(BID, Profit),

    ?INFO("Bid: ~s, Ask: ~s, Want: ~s", [bb:float2list(BID),  bb:float2list(ASK), bb:float2list(BestSellPrice)]),

    case ASK >= BestSellPrice of
        true ->
            gen_server:cast(bb_trade, {buy, Symbol, Quantity, BID, BestSellPrice, Remain});
        false -> ok
    end,
    {noreply, State};

handle_info({gun_ws_upgrade, _Pid, ok, _Headers}, State) ->
    ?INFO("Connection to stream server is upgraded to websocket"),
    {noreply, State};

handle_info(Msg, State) ->
    ?DEBUG("Got unexpected message: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, #state{pid = Pid} = _State) ->
    case is_pid(Pid) of
        true ->
            gun:shutdown(Pid);
        false -> ok
    end,
    bb_trade:start_link(),
    bb_rest:start_link(),
    ok.

best_sell_price(BIDPrice, Profit) ->
    BIDPrice + (BIDPrice * Profit / 100). %% + (BIDPrice * (Quantity / 100 * bb:fee())).

ticker_path(#state{symbol = Symbol} = _State) ->
    "/ws/" ++ Symbol ++ "@ticker".
