-module(test).
-export([start/0,stop/0,clear/1,clear/0, alloc/1, dealloc/2]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
       spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      timer:sleep(2000),
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      timer:sleep(2000),
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

%% Rename alloc and dealloc as to not colide with the internal functions


alloc(Timeout) -> 
    frequency ! {request, self(), allocate},
    receive 
      {reply, Reply} -> Reply
      after Timeout -> {error, 'Timeout reached'}
    end.

dealloc(Freq, Timeout) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
      {reply, Reply} -> Reply
      after Timeout -> {error, 'Timeout reached'}
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
      {reply, Reply} -> Reply
    end.

%% clear/0 to clear out the mailbox if there is something there when it is executed.
%% clear/1 to clear out the mailbox after a given timeout

clear() ->
  receive
    _Msg -> clear()
    after 0 -> ok
  end.
clear(Timeout) ->
  receive
    _Msg -> clear(Timeout)
    after Timeout -> {error, 'Timeout reached'}
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