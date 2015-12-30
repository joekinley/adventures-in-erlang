-module(server).
-behaviour(gen_server).
-include("defines.hrl").
-define(SERVER, ?MODULE).
-record(state, {games, listener}). % local record -> games: a list of all games, listener -> current listener
-record(gameinfo, {sockets, name, pid}). % holds game information, a list of sockets and the game name


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, listener_loop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  State = #state{games=[]},
  renew_listener(State).

handle_call({start_game, Socket}, _From, State) ->
  UniqueName = tools:get_random_string(8),
  case gen_server:start_link(game, [self(), Socket, UniqueName], []) of
    {ok, Pid} -> NewGame = #gameinfo{sockets=[Socket], name=UniqueName, pid=Pid},
                 NewState = State#state{games=State#state.games ++ [NewGame]},
                 {reply, ok, NewState};
    _         -> {reply, error, State}
  end;
handle_call({renew_listener}, _From, State) ->
  case renew_listener(State) of
    {ok, NewState} -> {reply, ok, NewState};
    {stop, Reason} -> {stop, Reason, State}
  end;
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

start_listening(Ip, Port) ->
  gen_tcp:listen(Port, [binary, {active, true}, {packet, 0}, {reuseaddr, true}, {ip, Ip}]).

renew_listener(State) ->
  Loop = spawn_link(?SERVER, listener_loop, [self()]),
  NewState = State#state{listener=Loop},
  {ok, NewState}.

listener_loop(From) ->
  case start_listening(?IP, ?PORT) of
    {ok, ListenSocket} -> case gen_tcp:accept(ListenSocket) of
                            {ok, Socket}    -> gen_server:call(From, {start_game, Socket}),
                                               gen_server:call(From, {renew_listener});
                            {error, Reason} -> {stop, Reason}
                          end;
    _                  -> gen_server:call(From, {renew_listener})
  end.

