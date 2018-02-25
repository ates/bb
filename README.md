bb - Simple Binance exchange trader Bot
---------------------------------------


How to use
----------

Erlang OTP 20 is required

Edit config/dev.config and specify your API Key and API Secret


```
    rebar3 shell
    
    bb:start(#{symbol => "gtobtc", quantity => 60, profit => 0.5, remain => 0.39}).
```
