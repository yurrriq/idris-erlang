-module(idris_erlang_conc).

-export([receive_any/0,receive_any_from/1,receive_any_msg/1]).
-export([receive_from/1,send/2]).
-export([rpc_send_req/2,rpc_recv_rep/1,rpc_recv_req/0,rpc_send_rep/2]).

-type from() :: {pid(),reference()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(IDRIS_MSG(From,Msg), {'$idris_rts_msg', From, Msg}).

%% This is ugly, but required for messaging beneath.  I could add a
%% timeout to make it safer, but then race conditions and no blocking,
%% so nope. Maybe I'll work out a better way.
-spec receive_any() -> {pid(),any()}.
receive_any() -> receive ?IDRIS_MSG(From, Msg) -> {From,Msg} end.

receive_any_from({'$idris_rts_any', From, _Msg}) -> From.

receive_any_msg({'$idris_rts_any', _From, Msg}) -> Msg.

-spec receive_from(pid()) -> any().
receive_from(Process) -> receive ?IDRIS_MSG(Process,Msg) -> Msg end.

-spec send(pid(), any()) -> any().
send(Process, Msg) ->
  Process ! ?IDRIS_MSG(self(), Msg),
  {}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RPC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(IDRIS_RPC_REQ(From, Message), {'$idris_rts_rpc_req',From,Message}).
-define(IDRIS_RPC_REP(Tag, Message), {'$idris_rts_rpc_rep',Tag,Message}).

-spec rpc_send_req(pid(), any()) -> reference().
rpc_send_req(Pid, Request) ->
  UniqueRef = make_ref(),
  Pid ! ?IDRIS_RPC_REQ({self(),UniqueRef}, Request),
  UniqueRef.

-spec rpc_recv_rep(reference()) -> any().
rpc_recv_rep(UniqueRef) ->
  receive ?IDRIS_RPC_REP(UniqueRef, Reply) -> Reply end.

-spec rpc_recv_req() -> {from(),any()}.
rpc_recv_req() -> receive ?IDRIS_RPC_REQ(From,Request) -> {From,Request} end.

-spec rpc_send_rep(from(), any()) -> {}.
rpc_send_rep(From, Reply) ->
  {Pid,UniqueRef} = From,
  Pid ! ?IDRIS_RPC_REP(UniqueRef, Reply),
  {}.
