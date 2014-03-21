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
	 init_event/2,
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
    {Fs,_} = proplists:split(Flags, [smsc,rp,udhi,udh,srr,mref,
				     vpf,vp,addr,pid,dcs,type,class,
				     alphabet,compression,store,wait_type,
				     notify,ref]),
    Opts = lists:append(Fs),
    gsms:send(Opts, Body1).

%%
%% init_event(in | out, Flags::[{atom(),term()}])
%%
init_event(_, _) ->
    ok.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(in, Flags) ->
    case hex:validate_flags(Flags, input_spec()) of
	ok -> ok;
	Error={error,Es} ->
	    lager:debug("validate_event: input option error ~p", [Es]),
	    Error
    end;
validate_event(out, Flags) ->
    case hex:validate_flags(Flags, output_spec()) of
	ok -> ok;
	Error={error,Es} ->
	    lager:debug("validate_event: output option error ~p", [Es]),
	    Error
    end.

input_spec() ->
    [ {reg_exp, optional, string, ""} |
      spec() ].

output_spec() ->     
    spec().

spec() ->
    [ {body, mandatory, string, ""},
      {smsc, optional, fun is_msisdn/1, ""},
      {rp, optional, boolean, false },
      {udhi, optional, boolean, false },
      {udh, optional, fun is_udh/1, []},
      {srr, optional, boolean, false },
      {mref,optional, unsigned8, 0},
      {vpf,optional,{alt,[{const,none},{const,relative},
			  {const,enhanced},{const,absolute}]}, none},
      {vp,optional,{alt,[{const,none},
			 {tuple,[{const,relative},unsigned]},
			 {tuple,[{const,absolute},
				 fun gsms_codec:is_valid_scts/1]}
			 %%{enhanced,_} fixme
			]},none},
      {addr, optional, fun is_msisdn/1, ""},
      {pid, optional, unsigned8, 0},
      {dcs, optional, fun is_dcs/1, 0},
      {type, optional, {alt,[{const,message},{const,data},
			    {cost,message_waiting}]}, message},
      {class, optional, {alt,[{const,alert},{const,me},
			      {const,sim},{const,te}]}, alert},

      {alphabet, optional, {alt,[{const,default},
				 {const,octet},
				 {const,ucs2},
				 {const,reserved}
				]}, default},
      {compression, optional,{alt,[{const,compressed},
				   {const,uncompressed}]}, uncompressed},

      {wait_type, optional, {alt,[{const,voicemail},{const,fax},
				  {const,email},{const,other}]}, other},
      {ref, optional, unsigned16, 1}
    ].


is_udh(UDH) ->
    try gsms_codec:encode_udh(UDH) of
	_Data -> true
    catch
	error:_ -> false
    end.

is_msisdn(Number) when is_list(Number) ->
    %% FIXME: check valid number codes
    true;
is_msisdn(_) ->
    false.

is_dcs(Value) when is_integer(Value) ->
    try gsms_codec:decode_dcs(Value) of
	_Dcs -> true
    catch
	error:_ -> false
    end.
