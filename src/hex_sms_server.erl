%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_sms_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([add_event/3, del_event/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(sub,
	{
	  ref     :: reference(),
	  mon     :: reference(),
	  filter  :: [{atom(),term()}],
	  signal  :: term(),
	  callback :: atom() | function()
	}).

-record(state, {
	  subs = []
	 }).

%%%===================================================================
%%% API
%%%===================================================================
add_event(Flags, Signal, Cb) ->
    gen_server:call(?MODULE, {add_event, self(), Flags, Signal, Cb}).

del_event(Ref) ->
    gen_server:call(?MODULE, {del_event, Ref}).

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    hex:auto_join(hex_sms),  %%  possibly signal to hex that we started
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_event,Pid,Flags,Signal,Cb}, _From, State) ->
    {Fs,_Pattern1} = proplists:split(Flags, [type,class,alphabet,pid,src,dst,
					     anumber,bnumber,smsc,reg_exp,
					     rssi,creg]),
    Filter = lists:append(Fs),
    case gsms:subscribe(Filter) of
	{ok,Ref} ->
	    Mon = erlang:monitor(process, Pid),
	    Sub = #sub { ref = Ref, 
			 mon = Mon,
			 filter = Filter,
			 signal = Signal,
			 callback = Cb },
	    {reply, {ok,Ref}, State#state { subs = [Sub|State#state.subs] }};
	Error ->
	    {reply, Error, State}
    end;
handle_call({del_event,Ref}, _From, State) ->
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {reply, {error, not_found}, State};
	{value, Sub, Subs} ->
	    gsms:unsubscribe(Ref),
	    erlang:demonitor(Sub#sub.mon, [flush]),
	    {reply, ok, State#state { subs=Subs} }
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({gsms, Ref, Pdu}, State) ->
    case lists:keyfind(Ref, #sub.ref, State#state.subs) of
	false ->
	    {noreply, State};
	Sub ->
	    case Pdu of
		{rssi,Value} ->
		    callback(Sub#sub.callback, Sub#sub.signal, [{value,Value}]);
		{creg,Value} ->
		    callback(Sub#sub.callback, Sub#sub.signal, [{value,Value}]);
		_ ->
		    callback(Sub#sub.callback, Sub#sub.signal, [{pdu,Pdu}])
	    end,
	    {noreply, State}
    end;
handle_info({'DOWN',Mon,process,_Pid,_Reason}, State) ->
    case lists:keytake(Mon, #sub.mon, State#state.subs) of
	false ->
	    {noreply, State};
	{value, Sub, Subs} ->
	    gsms:unsubscribe(Sub#sub.ref),
	    {noreply, State#state { subs=Subs} }
    end;
handle_info(_Info, State) ->
    lager:debug("got info ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    lists:foreach(
      fun(Sub) ->
	      gsms:unsubscribe(Sub#sub.ref)
      end, State#state.subs),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

callback(Cb,Signal,Env) when is_atom(Cb) ->
    Cb:event(Signal, Env);
callback(Cb,Signal,Env) when is_function(Cb, 2) ->
    Cb(Signal,Env).
