-module(frequency).
-export([start/0, init/0, clear/0, client/2, client/0]).
-export([allocate/0, deallocate/1, stop/0, available_frequencies/0, test_overload_server/0]).


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.
allocate({[], Allocated}, _Pid) ->
  {
    {[], Allocated},
    {error, no_frequency}
  };

allocate({[Freq| Free] , Allocated}, Pid) ->
  link(Pid),
  {
    {Free, [{Freq, Pid} | Allocated]},
    {ok, Freq}
  }.

deallocate({Free, Allocated}, Freq) ->
  {value, {Freq, Pid}} = lists:keysearch(Freq, 1, Allocated),
  unlink(Pid),
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq | Free], NewAllocated}.


%% Functional interface
% I chose to clear the mailbox at each client query

allocate() ->
    clear(),
    frequency ! {request, self(), allocate},
    receive
      {reply, Reply} -> 
        io:format("Reply ~w ~n", [Reply]),
        Reply
      after 1000 ->
        io:format("the server is overloaded, request  failed~n")
    end.

deallocate(Freq) ->
    clear(),
    frequency ! {request, self(), {deallocate, Freq}},
    receive
      {reply, Reply} -> 
      io:format("Reply ~w ~n", [Reply]),
      Reply
    after 1000 ->
      io:format("the server is overloaded, request  failed~n")
    end.

stop() ->
    clear(),
    frequency ! {request, self(), stop},
    receive
      {reply, Reply} -> 
      io:format("Reply ~w ~n", [Reply]),
      Reply
    after 1000 ->
      io:format("the server is overloaded, request  failed~n")
    end.

% add a tool function to fetch available frequencies
available_frequencies() ->
  clear(),
  frequency ! {request, self(), server_list},
  receive
      {reply, Reply} -> Reply
  after 1000 ->
    io:format("the server is overloaded, request  failed~n")
  end.

% it allows a client to deallocate a frequency that it is not currently using.
% I remove the validation in allocation (a client to hold more than one frequency)
check_and_allocate({_Free, Allocated}, Pid) ->
  case lists:keysearch(Pid, 2, Allocated) of
    false -> allocate({_Free, Allocated}, Pid);
    _ -> { {_Free, Allocated}, {error, already_allocated}}
  end.


check_and_deallocate({_Free, Allocated}, Freq, Pid) ->
  ElementFound = lists:keysearch(Freq, 1, Allocated),
  case validate_deallocate(ElementFound, Pid) of
    true -> {deallocate({_Free, Allocated}, Freq), {ok, Freq}};
    false -> {{_Free, Allocated}, {error, cannot_destroy}}
  end.


validate_deallocate({_, {_Freq, Pid}}, Pid) -> true;

validate_deallocate({_, {_Freq, _PidNode}}, _Pid) -> false;

validate_deallocate(false, _Pid) -> false.

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      { NewFrequencies, Reply } = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);

    {request, Pid, {deallocate, Freq}} ->
      {NewFrequencies, Reply} = check_and_deallocate(Frequencies, Freq, Pid),
      Pid ! { reply, Reply },
      loop(NewFrequencies);

    {request, Pid, server_list} ->
      {FreeFrequences, _} = Frequencies,
      Pid ! { reply,  FreeFrequences},
      loop(Frequencies);

    % catch a possible error
    {'EXIT', Pid, _Reason} ->
      NewFrequencies = exited(Frequencies, Pid),
      loop(NewFrequencies);

    {request, Pid, stop} ->
      Pid ! { reply, stopped }
  end.


exited({Free, Allocated}, Pid) ->
  case lists:keysearch(Pid, 2, Allocated) of
    {value, {Freq, Pid}} ->
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free], NewAllocated};
    false ->
      {Free, Allocated}
  end.

start() ->
  ServerPid = spawn(frequency, init, []),
  register(frequency, ServerPid).

init() ->
  process_flag(trap_exit, true), % atomic registration
  Frequencies = { get_frequencies(), [] },
  loop(Frequencies).

clear() ->
  receive
    _Msg ->
    io:format("Shell got ~w ~n", [_Msg]),
    clear()
  after 0 ->
    ok
  end.

% tool function
  get_frequencies() -> [10,11,12,13,14,15,16].

client() ->
  allocate(),
  timer:sleep(1000),
  Frequency = random:uniform(length(get_frequencies())) + 10,
  deallocate(Frequency).


client(0, 0) ->
  ok;

client(0, NbDealloc) ->
  Frequency = random:uniform(length(get_frequencies())) + 10,
  deallocate(Frequency),
  client(0, NbDealloc -1);

client(NbAlloc, NbDealloc) ->
  allocate(),
  client(NbAlloc -1, NbDealloc).


% function to show the problem with an overload server
test_overload_server() ->
  start(),
  allocate(),
  allocate(),
  allocate(),
  allocate(),
  allocate(),
  allocate(),
  io:format("Wait (6 * 5000) = 30 seconds~n"),
  % will show messages
  timer:sleep(30000),
  clear(),
  frequency:stop().
