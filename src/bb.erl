-module(bb).

-export([start/1]).
-export([float2list/1]).
-export([float2list/2]).
-export([float2binary/1]).
-export([from_json/1]).
-export([get_env/1]).
-export([get_env/2]).
-export([timestamp/0]).
-export([signature/2]).
-export([to_binary/1]).
-export([fee/0]).

start(Opts) ->
    bb_stream:start_link(Opts).

float2list(Value) when is_float(Value) ->
    float2list(Value, 8).

float2list(Value, N) ->
    erlang:float_to_list(Value, [{decimals, N}]).

float2binary(Value) when is_float(Value) ->
    erlang:float_to_binary(Value, [{decimals, application:get_env(bb, d, 8)}]).

from_json(Data) ->
    jsx:decode(Data, [return_maps, {labels, atom}]).

get_env(Option) ->
    get_env(Option, undefined).

get_env(Option, Default) ->
    application:get_env(bb, Option, Default).

timestamp() ->
    erlang:system_time(milli_seconds).

signature(Query, Secret) ->
    Hash = crypto:hmac(sha256, Secret, Query),
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [N]) || <<N:8>> <= Hash])).

to_binary(Data) when is_list(Data) ->
    list_to_binary(Data);
to_binary(Data) when is_atom(Data) ->
    atom_to_binary(Data, latin1);
to_binary(Data) when is_integer(Data) ->
    integer_to_binary(Data);
to_binary(Data) when is_float(Data) ->
    float_to_binary(Data);
to_binary(Data) when is_binary(Data) ->
    Data.

fee() ->
    application:get_env(?MODULE, fee, 0.10).
