-module(bb_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SPEC(M), #{id => M, start => {M, start_link, []}}).
-define(CHILDS, [bb_currency]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, [?SPEC(C) || C <- ?CHILDS]}}.
