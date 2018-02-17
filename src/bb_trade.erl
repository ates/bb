-module(bb_trade).

-behaviour(gen_server).

-export([start_link/0]).
-export([stop/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("bb.hrl").

-record(state, {
    symbol              :: string(),
    ready_to_buy = true :: boolean(),
    sell_price = 0.0    :: float(),
    buy_price = 0.0     :: float(),
    order_id            :: {buy | sell, pos_integer()},
    quantity = 0        :: pos_integer() | float(),
    buy_quantity = 0    :: pos_integer() | float(),
    last_quantity = 0   :: pos_integer() | float(),
    n = 0               :: non_neg_integer(),
    remain = 0          :: non_neg_integer() | float()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([]) ->
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({buy, Symbol, Quantity, BuyPrice, SellPrice, Remain}, State) when State#state.ready_to_buy =:= true ->
    Order = #{
        symbol   => Symbol,
        side     => <<"BUY">>,
        quantity => Quantity,
        price    => bb:float2binary(BuyPrice)
    },
    case bb_rest:new_order(Order) of
        {ok, #{orderId := OrderId}} ->
            ?INFO("Order created, side: buy, quantity: ~p, price: ~s, order_id: ~p", [
                Quantity, bb:float2list(BuyPrice), OrderId
            ]),
            NewState =
                State#state{
                    symbol       = Symbol,
                    buy_quantity = Quantity,
                    quantity     = Quantity,
                    sell_price   = SellPrice,
                    buy_price    = BuyPrice,
                    order_id     = {buy, OrderId},
                    ready_to_buy = false,
                    remain       =
                        case State#state.order_id of
                            undefined -> Remain;
                            _ -> State#state.remain
                        end
                },
            erlang:send_after(1000, self(), check_order),
            {noreply, NewState};
        Error ->
            ?ERROR("Can't create order, order: ~p, error: ~p", [Order, Error]),
            {noreply, State#state{ready_to_buy = false}}
    end;

handle_cast({buy, _Symbol, _Quantity, _BuyPrice, _SellPrice}, State) ->
    ?INFO("Can't create new order, side: buy, existed order_id: ~p", [State#state.order_id]),
    {noreply, State};

handle_cast(sell, State) ->
    #state{
        symbol     = Symbol,
        quantity   = Quantity,
        sell_price = Price
    } = State,
    RoundRemain = trunc(State#state.remain),
    {NewQuantity, Remain} =
        case RoundRemain >= 1 of
            true ->
                {Quantity + RoundRemain, State#state.remain - RoundRemain};
            false ->
                {Quantity, State#state.remain}
        end,
    Order = #{
        symbol   => Symbol,
        side     => <<"SELL">>,
        quantity => NewQuantity,
        price    => bb:float2binary(Price)
    },
    case bb_rest:new_order(Order) of
        {ok, #{orderId := OrderId}} ->
            ?INFO("Order created, side: sell, quantity: ~p, price: ~s, remain: ~s, order_id: ~p", [
                NewQuantity, bb:float2list(Price), bb:float2list(Remain), OrderId
            ]),
            NewState = State#state{
                order_id      = {sell, OrderId},
                remain        = Remain,
                last_quantity = NewQuantity
            },
            erlang:send_after(1000, self(), check_order),
            {noreply, NewState};
        Error ->
            ?ERROR("Can't create order, order: ~p, error: ~p", [Order, Error]),
            {noreply, State}
    end;

handle_cast(_Request, State) -> {noreply, State}.

handle_info(check_order, State) ->
    #state{
        n        = N,
        symbol   = Symbol,
        order_id = {Side, OrderId}
    } = State,
    Order = #{symbol => Symbol, order_id => OrderId},

    case bb_rest:query_order(Order) of
        {ok, #{status := <<"NEW">>}} when N =:= 10, Side =:= buy ->
            {ok, Result} = bb_rest:delete_order(Order),
            ?INFO("Order is expired, side: buy, order_id: ~p, action: delete, result: ~p", [OrderId, Result]),
            NewState = State#state{
                n            = 0,
                ready_to_buy = true
            },
            {noreply, NewState};
        {ok, #{status := <<"NEW">>}} when N < 10, Side =:= buy ->
            ?INFO("Order is not filled, side: buy, order_id: ~p", [OrderId]),
            erlang:send_after(1000, self(), check_order),
            {noreply, State#state{n = N + 1}};
        {ok, #{status := <<"NEW">>}} when Side =:= sell ->
            ?INFO("Order is not filled, side: sell, order_id: ~p", [OrderId]),
            erlang:send_after(1000, self(), check_order),
            {noreply, State};
        {ok, #{status := <<"PARTIALLY_FILLED">>}} ->
            ?INFO("Order is partially filled, side: ~p, order_id: ~p", [Side, OrderId]),
            erlang:send_after(1000, self(), check_order),
            {noreply, State};
        {ok, #{status := <<"CANCELED">>}} ->
            ?INFO("Order is cancelled, order_id: ~p", [OrderId]),
            {noreply, State#state{ready_to_buy = false}};
        {ok, #{status := <<"FILLED">>}} when Side =:= buy ->
            ?INFO("Order is filled, side: buy, order_id: ~p", [OrderId]),
            Fee = State#state.quantity / 100 * bb:fee(),
            NewQuantity = State#state.quantity - Fee,
            NewState = State#state{
                quantity = trunc(NewQuantity),
                remain   = State#state.remain + (NewQuantity - trunc(NewQuantity))
            },
            gen_server:cast(?MODULE, sell),
            {noreply, NewState};
        {ok, #{status := <<"FILLED">>}} when Side =:= sell ->
            #state{
                buy_quantity  = BuyQuantity,
                last_quantity = LastQuantity, %% Sell quantity
                buy_price     = BuyPrice,
                sell_price    = SellPrice
            } = State,
            Total = LastQuantity * SellPrice,
            Fee = Total / 100 * bb:fee(),
            Profit = (Total - (BuyQuantity * BuyPrice)) - Fee,
            USD = bb_currency:usd_to_btc() * Profit,
            ?INFO(
                "Order is filled, side: sell, q: ~p, price: ~s, total: ~s, fee: ~s btc = ~s $, profit: ~s btc = ~s $, order_id: ~p", [
                LastQuantity, bb:float2list(SellPrice), bb:float2list(Total),
                bb:float2list(Fee),
                bb:float2list(bb_currency:usd_to_btc() * Fee, 2),
                bb:float2list(Profit), bb:float2list(USD, 2), OrderId
            ]),
            {noreply, State#state{n = 0, ready_to_buy = true}}
    end;

handle_info(Msg, State) ->
    ?DEBUG("Msg: ~p~n", [Msg]),
    {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
