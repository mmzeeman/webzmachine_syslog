%%
%% @doc HTTP access log, sending to syslog over UDP
%%

-module(webzmachine_syslog).

-export([log/2]).

%%
%%
%%

log(Msg, Name) ->
    webzmachine_syslog_server:log(Name, Msg),
    ok.


