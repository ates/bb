-module(bb_rest).  
-behaviour(gen_server).

-export([start_link/0]).
-export([stop/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-export([new_order/1]).
-export([delete_order/1]).
-export([query_order/1]).
-export([account/0]).

-include("bb.hrl").

-record(state, {
    pid                :: undefined | pid(),
    recv_window = 5000 :: pos_integer(),
    api_key = <<>>     :: binary(),
    api_secret = <<>>  :: binary()
}).

stop() ->
    gen_server:call(?MODULE, stop).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    URL = bb:get_env(rest_url),
    Port = bb:get_env(rest_port),
    GunOpts = #{
        protocols => [http],
        transport => ssl,
        http_opts => #{keepalive => 2000}
    },
    {ok, Pid} = gun:open(URL, Port, GunOpts),
    RecvWindow = application:get_env(bb, recv_window, 5000),
    {ok, Key} = application:get_env(bb, api_key),
    {ok, Secret} = application:get_env(bb, api_secret),
    State = #state{
        pid = Pid,
        recv_window = RecvWindow,
        api_key = Key,
        api_secret = Secret
    },
    {ok, State}.

handle_call({new_order, Order}, _From, State) ->
    Reply = post(make_path(new_order), Order, State),
    {reply, Reply, State};

handle_call({delete_order, Order}, _From, State) ->
    Reply = req(delete, make_path(delete_order), Order, State),
    {reply, Reply, State};

handle_call({query_order, Data}, _From, State) ->
    Reply = req(get, make_path(query_order), Data, State),
    {reply, Reply, State};

handle_call(account, _From, State) ->
    Reply = req(get, make_path(account), <<>>, State),
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.

handle_info(Msg, State) ->
    ?DEBUG("Got ~p", [Msg]),
    {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, #state{pid = Pid} = _State) ->
    case is_pid(Pid) of
        true ->
            gun:shutdown(Pid);
        false -> ok
    end.

new_order(Order) ->
    #{
        symbol   := Symbol,
        side     := Side,
        quantity := Quantity,
        price    := Price
    } = Order,
    Data = <<
        "symbol=",
        (bb:to_binary(string:to_upper(Symbol)))/binary,
        "&side=",
        Side/binary,
        "&type=LIMIT",
        "&quantity=",
        (bb:to_binary(Quantity))/binary,
        "&timeInForce=GTC",
        "&price=",
        Price/binary
    >>,
    gen_server:call(?MODULE, {new_order, Data}).

delete_order(#{symbol := Symbol, order_id := OrderId}) ->
    Data = <<
        "symbol=",
        (bb:to_binary(string:to_upper(Symbol)))/binary,
        "&orderId=",
        (bb:to_binary(OrderId))/binary
    >>,
    gen_server:call(?MODULE, {delete_order, Data}).

query_order(#{symbol := Symbol, order_id := OrderId}) ->
    Data = <<
        "symbol=",
        (bb:to_binary(string:to_upper(Symbol)))/binary,
        "&orderId=",
        (bb:to_binary(OrderId))/binary
    >>,
    gen_server:call(?MODULE, {query_order, Data}).

account() ->
    gen_server:call(?MODULE, account).

req(Method, URI, Data, State) ->
    Headers = [
        {<<"X-MBX-APIKEY">>, State#state.api_key}
    ],
    NewData = <<
        Data/binary,
        "&recvWindow=",
        (bb:to_binary(State#state.recv_window))/binary,
        "&timestamp=",
        (bb:to_binary(bb:timestamp()))/binary
    >>,
    DataWithSign = <<
        NewData/binary,
        "&signature=",
        (bb:signature(NewData, State#state.api_secret))/binary
    >>,
    NewURI = <<URI/binary, "?", DataWithSign/binary>>,
    StreamRef = gun:Method(State#state.pid, NewURI, Headers),
    wait_for_response(StreamRef, State).

post(URI, Data, State) ->
    Headers = [
        {<<"X-MBX-APIKEY">>, State#state.api_key}
    ],
    NewData = <<
        Data/binary,
        "&recvWindow=",
        (bb:to_binary(State#state.recv_window))/binary,
        "&timestamp=",
        (bb:to_binary(bb:timestamp()))/binary
    >>,
    DataWithSign = <<
        NewData/binary,
        "&signature=",
        (bb:signature(NewData, State#state.api_secret))/binary
    >>,
    StreamRef = gun:post(State#state.pid, URI, Headers, DataWithSign),
    wait_for_response(StreamRef, State).

wait_for_response(StreamRef, #state{pid = Pid} = State) ->
    receive
        {gun_response, Pid, StreamRef, fin, _Status, _Headers} ->
            {error, no_data};
        {gun_response, Pid, StreamRef, nofin, Status, _Headers} ->
            case collect_response(StreamRef, State, <<>>) of
                {ok, Data} when Status =:= 200 ->
                    {ok, Data};
                {ok, Error} when Status =/= 200 ->
                    {error, #{reason => Error, status => Status}};
                Error -> Error
            end
    after 5000 ->
        {error, timeout}
    end.

collect_response(StreamRef, #state{pid = Pid} = State, Acc) ->
    receive
        {gun_data, Pid, StreamRef, nofin, Data} ->
            collect_response(StreamRef, State, <<Acc/binary, Data/binary>>);
        {gun_data, Pid, StreamRef, fin, Data} ->
            {ok, bb:from_json(<<Acc/binary, Data/binary>>)}
    after 5000 ->
        {error, timeout}
    end.

make_path(new_order) -> <<"/api/v3/order">>;
make_path(delete_order) -> <<"/api/v3/order">>;
make_path(query_order) -> <<"/api/v3/order">>;
make_path(account) -> <<"/api/v3/account">>.
