-module(bb_currency).

-behaviour(gen_server).

-export([start_link/0]).
-export([usd_to_btc/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("bb.hrl").

-record(state, {
    pid       :: undefined | pid(),
	timer	  :: reference(),
    usd = 0.0 :: float()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

usd_to_btc() ->
    gen_server:call(?MODULE, usd_to_btc).

init([]) ->
    URL = bb:get_env(currency_url),
    GunOpts = #{
        protocols => [http],
        transport => ssl,
        http_opts => #{keepalive => 2000}
    },
    {ok, Pid} = gun:open(URL, 443, GunOpts),
    Timer = erlang:send_after(5000, self(), get_currency),
    {ok, #state{pid = Pid, timer = Timer}}.

handle_call(usd_to_btc, _From, State) ->
    {reply, State#state.usd, State};
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.

handle_info({gun_up, _Pid, http}, State) ->
    {noreply, State};

handle_info(get_currency, State) ->
    erlang:cancel_timer(State#state.timer),
    StreamRef = gun:get(State#state.pid, "/v1/bpi/currentprice/BTC.json"),
    USD =
        case wait_for_response(StreamRef, State) of
            {ok, #{bpi := #{'USD' := #{rate_float := Rate}}}} -> Rate;
            _ -> State#state.usd
        end,
    Timer = erlang:send_after(5000, self(), get_currency),
    {noreply, State#state{usd = USD, timer = Timer}};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, #state{pid = Pid} = _State) ->
    case is_pid(Pid) of
        true ->
            gun:shutdown(Pid);
        false -> ok
    end.

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
