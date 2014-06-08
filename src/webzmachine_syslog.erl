%%
%% @doc HTTP access log, sending to syslog over UDP
%%

-module(webzmachine_syslog).

-include_lib("webzmachine/include/wm_reqdata.hrl").
-include_lib("webzmachine/include/webmachine_logger.hrl").

%%
%%
%%

log_access(#wm_log_data{}=LogData, Name) ->
    Msg = format_req(LogData),
    webzmachine_syslog:log(Name, Msg),
    ok.

format_req(_LogData) ->
    "msg".

