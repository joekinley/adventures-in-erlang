-module(game).
-behaviour(gen_server).
-include("defines.hrl").
-define(SERVER, ?MODULE).
-record(state, {communications, name, map, server}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, listen_socket/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([From, Communication, Name]) ->
  io:format("New game started~n", []),
  State = #state{communications=[Communication], name=Name, server=From},
  gen_server:call(Communication, {send_message, "Ready Player One\n"}),
  {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

listen_socket(_From, Socket) ->
  gen_tcp:send(Socket, "Waiting\n"),
  io:format("Waiting 1~n", []),
  Res = gen_tcp:recv(Socket, 0),
  io:format("Waiting done answer: ~p~n", [Res]),
  ok.
% starts the game for the given username
%start_game(Username).

% initializes a brand new game for the given username
%init_Game(Username).


