%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%%
%% Exercice 3.4
%%

-module(frequency).
-export([start_router/0, start/1, allocate/0, deallocate/1, stop/0, get_frequencies/1]).
-export([init/1, init_router/0, next_server/2]).

%% These are the start functions used to create and
%% initialize the server.

start(RangeFrequencies) ->
  spawn(frequency, init, [get_frequencies(RangeFrequencies)]).

init(Freqs) ->
  Frequencies = {Freqs, []},
  loop(Frequencies).

% TODO allows more than two nodes
start_router() ->
  Pid = spawn(frequency, init_router, []),
  register(frequency, Pid).

init_router() ->
  ServerPid1 = start(1),
  ServerPid2 = start(2),
  loop_router([ServerPid1, ServerPid2], 1).


get_frequencies(N) ->
  lists:map((fun(X)-> X + N * 10 end),[0, 1, 2, 3, 4, 5]).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

loop_router(Servers, Index) ->
  receive
    {request, Pid, allocate} ->
      ServAllocator = get_server_by_index(Servers, Index),
      ServAllocator ! {request, Pid, allocate},
      loop_router(Servers, next_server(Servers, Index));
    {request, Pid, {deallocate, Freq}} ->
      ServPid = get_server_pid(Freq, Servers),
      ServPid ! {request, Pid, {deallocate, Freq}},
      loop_router(Servers, Index);
    {request, Pid, stop} ->
      stop_servers(Servers),
      Pid ! {reply, stopped}
  end.


get_server_pid(Freq, Servers) when Freq < 10 ->
  lists:nth(1, Servers);

get_server_pid(Freq, Servers) ->
  lists:nth(Freq div 10, Servers).

next_server(Servers, Index) ->
  case (Index + 1) > length(Servers) of
      true -> 1;
      false -> Index + 1
  end.

get_server_by_index(Servers, Index) ->
  lists:nth(Index, Servers).


stop_servers([Server| Servers]) ->
  exit(Server, kill),
  stop_servers(Servers);

stop_servers([]) ->
  ok.

%% The Main Loop
loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() ->
    frequency ! {request, self(), allocate},
    receive
      {reply, Reply} -> Reply
    end.

deallocate(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    receive
      {reply, Reply} -> Reply
    end.

stop() ->
    frequency ! {request, self(), stop},
    receive
      {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.
