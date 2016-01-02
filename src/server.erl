-module(server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {games}). % local record -> games: a list of all games, listener -> current listener
-record(gameinfo, {name, pid}). % holds game information, a list of sockets and the game name


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

handle_call({start_game, CommPid}, _From, State) ->
  UniqueName = tools:get_random_string(8),
  case gen_server:start_link(game, [self(), CommPid, UniqueName], []) of
    {ok, Pid} -> NewGame = #gameinfo{name=UniqueName, pid=Pid},
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

renew_listener(State) ->
  gen_server:start_link(communication, [self()], []),
  {ok, State}.
