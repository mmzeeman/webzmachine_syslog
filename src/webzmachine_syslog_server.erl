%%
%%
%%

-module(webzmachine_syslog_server).
-behaviour(gen_server).

%% Collect log messages in ets and flush them every second.

%% API
-export([start_link/2, log/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name, msg_opts, log}).

%%
%% API
%%

start_link(Name, MsgOpts) when is_atom(Name) ->
    gen_server:start_link({local, Name}, Name, [Name, MsgOpts], []).

log(Name, Msg) ->
    (catch ets:insert(Name, {{os:timestamp(), make_ref()} , Msg})).

%%
%% gen_server callbacks
%%

init([Name, MsgOpts]) ->
    Name = ets:new(Name, [ordered_set, named_table, public,
            {write_concurrency, true}]),
    {ok, Log} = syslog:open(ident(MsgOpts), [], facility(MsgOpts)),
    erlang:send_after(1000, self(), flush),
    {ok, #state{name=Name, msg_opts=MsgOpts, log=Log}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, #state{log=Log}=State) ->
    log_before(Log, os:timestamp()),
    erlang:send_after(1000, self(), flush),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% INTERNAL
%%

log_before(Log, Now) ->
    log_before(Log, Now, ets:first(?MODULE)).

log_before(_Log, _Now, '$end_of_table') -> 
    ok;
log_before(Log, Now, {MsgTs, _Ref}=Key) when Now >= MsgTs ->
    [{_, Msg}] = ets:lookup(?MODULE, Key),
    true = ets:delete(?MODULE, Key),
    syslog:log(Log, info, Msg),
    log_before(Log, Now, ets:next(?MODULE, Key));
log_before(_Log, _Now, _Key) -> 
    ok.

%%
%% HELPERS
%%

facility(Config) -> 
    proplists:get_value(facility, Config, local0).

ident(Config) -> 
    proplists:get_value(ident, Config, atom_to_list(node())).

