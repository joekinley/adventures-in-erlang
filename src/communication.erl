-module(communication).
-behaviour(gen_server).
-include("defines.hrl").
-define(SERVER, ?MODULE).
-record(state, {server, socket, game}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, listener_loop/2]).

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

init([Server]) ->
  State = #state{server=Server},
  spawn_link(?SERVER, listener_loop, [self(), Server]),
  {ok, State}.

handle_call({register_socket, Socket}, _From, State) ->
  NewState = State#state{socket=Socket},
  {reply, ok, NewState};
handle_call({send_message, Message}, _From, State) ->
  gen_tcp:send(State#state.socket, Message),
  {reply, ok, State};
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
  gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}, {ip, Ip}, {keepalive, true}]).

listener_loop(From, Server) ->
  case start_listening(?IP, ?PORT) of
    {ok, ListenSocket} -> case gen_tcp:accept(ListenSocket) of
                            {ok, Socket}    -> gen_tcp:controlling_process(Socket, From),
                                               gen_server:call(From, {register_socket, Socket}),
                                               gen_server:call(Server, {renew_listener}),
                                               gen_server:call(Server, {start_game, From});
                            {error, Reason} -> {stop, Reason}
                          end;
    _                  -> gen_server:call(Server, {renew_listener})
  end.