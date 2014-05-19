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
%%%    Hex SMS plugin 
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_sms).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 event_spec/1,
	 init_event/2,
	 mod_event/2,
	 add_event/3, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal(), 
%%            Cb::function()|atom()) ->
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(Flags, Signal, Cb) ->
    hex_sms_server:add_event(Flags, Signal, Cb).

%%
%%  del_event(Ref::reference()) ->
%%     ok.
del_event(Ref) ->
    hex_sms_server:del_event(Ref).
%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
output(Flags, Env) ->
    Body = proplists:get_value(body, Flags, ""),
    Body1  = hex:text_expand(Body, Env),
    %% special process udh headers
    UDH = get_udh(Flags),
    VP = get_vp(Flags),
    {Fs,_} = proplists:split(Flags, [smsc,rp,udhi,srr,mref,
				     vpf,addr,pid,dcs,type,class,
				     alphabet,compression,store,wait_type,
				     notify,ref]),
    Opts = UDH ++ VP ++ lists:append(Fs),
    gsms:send(Opts, Body1).

get_vp(Flags) ->
    case proplists:get_value(vp, Flags) of
	undefined -> [];
	none -> [{vp,none}];
	{relative,Value} -> [{vp,{relative,Value}}];
	{absolute,Value} -> [{vp,{absolute,get_scts(Value)}}]
    end.

-define(l2i(L), list_to_integer((L))).

get_scts([Y1,Y2,Y3,Y4,$-,Mon1,Mon2,$-,D1,D2,$T,H1,H2,$:,M1,M2,$:,S1,S2|Rest]) ->
    Z = case Rest of
	    [] -> 0.0;
	    [$Z,$+,Z1,Z2] -> ?l2i([Z1,Z2])/4;
	    [$Z,$-,Z1,Z2] -> -?l2i([Z1,Z2])/4
	end,
    { { {?l2i([Y1,Y2,Y3,Y4]),?l2i([Mon1,Mon2]),?l2i([D1,D2])},
	{?l2i([H1,H2]),?l2i([M1,M2]),?l2i([S1,S2])} }, Z}.
    

get_udh(Flags) ->
    UDH = lists:map(
	    fun(UDH) ->
		    case proplists:get_value(hdr,UDH) of
			{concat8,Opts} -> get_concat(concat8,Opts);
			{concat16,Opts} -> get_concat(concat16,Opts);
			{concat,Opts} -> get_concat(concat,Opts);
			{port8,Opts} -> get_port(port8,Opts);
			{port16,Opts} -> get_port(port16,Opts);
			{port,Opts} -> get_port(port,Opts)
		    end
	    end, proplists:get_all_values(udh, Flags)),
    if UDH =:= [] -> [];
       true -> [{udh,UDH}]
    end.

get_concat(Tag,Opts) ->
    {Tag,
     proplists:get_value(ref,Opts,0),
     proplists:get_value(total,Opts,0),
     proplists:get_value(segment,Opts,0)}.

get_port(Tag,Opts) ->
    {Tag,
     proplists:get_value(dst,Opts,0),
     proplists:get_value(src,Opts,0)}.

%%
%% init_event(in | out, Flags::[{atom(),term()}])
%%
init_event(_, _) ->
    ok.

%%
%% mod_event(in | out, Flags::[{atom(),term()}])
%%
mod_event(_, _) ->
    ok.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(Dir, Flags) ->
    hex:validate_flags(Flags, event_spec(Dir)).
    
event_spec(in) ->
    [ {leaf, reg_exp, [{type, string, []}, {default,"",[]}]},
      {leaf, rssi, [{type, boolean, []}, {default, false, []}]},
      {leaf, creg, [{type, boolean, []}, {default, false, []}]}
      | spec() ];
event_spec(out) ->
    spec().

spec() ->
    [ 
      {leaf, body, [{type, string, []}, {default, "", []}]},

      {leaf, smsc, [{type, string, 
		     [{pattern, "\\+?[0-9]*", []}]}
		   ]},

      {leaf, rp, [{type, boolean, []},
		  {default, false, []}]},

      {leaf, udhi, [{type, boolean,[]}, 
		    {default, false, []}]},

      {list, udh, [{key,id,[]},
		   {leaf,id,[{type,uint32,[]}]},
		   {choice,hdr,
		    [
		     %% concat are generated automatically, but may
		     %% sometimes be hacked for special purposes
		     {'case',concat8,
		      [{container,concat8,
			[{leaf,ref,[{type,uint8,[]}]},
			 {leaf,total,[{type,uint8,[]}]},
			 {leaf,segment,[{type,uint8,[]}]}]}]},
		     {'case',concat16,
		      [{container,concat16,
			[{leaf,ref,[{type,uint16,[]}]},
			 {leaf,total,[{type,uint8,[]}]},
			 {leaf,segment,[{type,uint8,[]}]}]}]},
		     {'case',concat, %% dynamically select concat8|concat16
		      [{container,concat,
			[{leaf,ref,[{type,uint16,[]}]},
			 {leaf,total,[{type,uint8,[]}]},
			 {leaf,segment,[{type,uint8,[]}]}]}]},
		     %% port numbers are used to adress special 
		     %% applications on mobile phones.
		     {'case',port8,
		      [{container,port8,
			[{leaf,dst,[{type,uint8,[]}]},
			 {leaf,src,[{type,uint8,[]}]}]}]},
		     {'case',port16,
		      [{container,port16,
			[{leaf,dst,[{type,uint16,[]}]},
			 {leaf,src,[{type,uint16,[]}]}]}]},
		     {'case',port, %% dynamically select port8|port16
		      [{container,port,
			[{leaf,dst,[{type,uint16,[]}]},
			 {leaf,src,[{type,uint16,[]}]}]}]}
		    ]}
		  ]},

      {leaf, srr, [{type, boolean, []}, {default, false, []}]},
      
      {leaf, mref, [{type, uint8, []}, {default, 0, []}]},

      {leaf, vpf, [{type, enumeration,
		    [{enum, none, []},
		     {enum, relative, []},
		     {enum, enhanced, []},
		     {enum, absolute, []}]},
		   {default, none, []}]},
		     
      {choice, vp,
       [{'case',none,[{leaf,none,[{type,empty,[]}]}]},
	{'case',relative,[{leaf,relative,[{type,uint32,[]}]}]},
	{'case',absolute,
	 [{leaf,absolute,
	   [{type,string,
	     [{pattern,"\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|[\\+\\-]\\d{2}:\\d{2})", []}]}]}]},
	%% {'case',enhanced,[{leaf,enhanced,[{type,'sms:enhanced',[]}]}]},
	{default,none,[]}]},

      {leaf, addr, [{type,string,
		     [{pattern, "\\+?[0-9]*", []}]}
		   ]},
      
      {leaf, pid, [{type, uint8, []}, {default,0,[]}]},

      {leaf, dcs, [{type, uint8, []}]},

      {leaf, type, 
       [{type,enumeration,[{enum,message,[]},
			   {enum,data,[]},
			   {enum,message_waiting,[]}]},
	{default, message, []}]},

      {leaf, class, 
       [{type,enumeration,[{enum,alert,[]},
			   {enum,me,[]},
			   {enum,sim,[]},
			   {enum,te,[]}]},
	{default, alert, []}]},

      {leaf, alphabet, 
       [{type,enumeration,[{enum,default,[]},
			   {enum,octet,[]},
			   {enum,ucs2,[]},
			   {enum,reserved,[]}]},
	{default, default, []}]},

      {leaf, compression, 
       [{type,enumeration,[{enum,compressed,[]},
			   {enum,uncompressed,[]}]},
	{default,uncompressed,[]}]},

      {leaf, wait_type, 
       [{type, enumeration, [{enum,voicemail,[]},
			     {enum,fax,[]},
			     {enum,email,[]},
			     {enum,other,[]}]},
       {default,  other, []}]},
      
      {leaf, ref, [{type,uint16,[]},
		   {default,1,[]}]}
    ].
