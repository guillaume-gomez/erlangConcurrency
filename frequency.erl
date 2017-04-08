-module(frequency).
-export([allocate/0, deallocate/1, stop/0, start/0, init/0, test_server_overload/0, test_with_cleared_server/0, clear/0]).


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.
allocate({[], Allocated}, _Pid) ->
  {
    {[], Allocated},
    {error, no_frequency}
  };

allocate({[Freq| Free] , Allocated}, Pid) ->
  {
    {Free, [{Freq, Pid} | Allocated]},
    {ok, Freq}
  }.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq, Free], NewAllocated}.


%% Functional interface

allocate() ->
    frequency ! {request, self(), allocate},
    receive
      {reply, Reply} -> Reply
      after 1000 ->
        io:format("the server is overloaded, request  failed~n")
    end.

deallocate(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    receive
      {reply, Reply} -> Reply
    after 1000 ->
      io:format("the server is overloaded, request  failed~n")
    end.

stop() ->
    frequency ! {request, self(), stop},
    receive
      {reply, Reply} -> Reply
    after 1000 ->
      io:format("the server is overloaded, request  failed~n")
    end.


% it allows a client to deallocate a frequency that it is not currently using.
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
  timer:sleep(2000),
  receive
    {request, Pid, allocate} ->
      { NewFrequencies, Reply } = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);

    {request, Pid, {deallocate, Freq}} ->
      {NewFrequencies, Reply} = check_and_deallocate(Frequencies, Freq, Pid),
      Pid ! { reply, Reply },
      loop(NewFrequencies);

    {request, Pid, stop} ->
      Pid ! { reply, stopped }
    after 2000 ->
      %spawn(frequency, clear, []),
      loop(Frequencies)
  end.

start() ->
  ServerPid = spawn(frequency, init, []),
  register(frequency, ServerPid).

init() ->
  Frequencies = { get_frequencies(), [] },
  loop(Frequencies).

clear() ->
  receive
    _Msg ->
    io:format("~w ~n", [_Msg]),
    clear()
  after 0 ->
    ok
  end.

clear_with_string() ->
  receive
    _Msg ->
    io:format("Shell got ~w ~n", [_Msg]),
    clear_with_string()
  after 0 ->
    ok
  end.

% tool function
  get_frequencies() -> [10,11,12,13,14,15,16].

test_server_overload() ->
  start(),
  allocate(),
  allocate(),
  allocate(),
  allocate(),
  allocate(),
  allocate(),
  io:format("Wait (6 * 2000) = 12 seconds~n"),
  % will show messages
  timer:sleep(12000),
  clear_with_string(),
  frequency:stop().


test_with_cleared_server() ->
  start(),
  allocate(),
  timer:sleep(2010),
  allocate(),
  timer:sleep(2010),
  allocate(),
  timer:sleep(2010),
  clear_with_string(),
  frequency:stop().




