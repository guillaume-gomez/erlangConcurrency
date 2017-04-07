-module(week1).
-include_lib("eunit/include/eunit.hrl").
-export([start/0, test/0, init/0, test_check_and_allocate/0, test_check_and_deallocate/0]).
-export([allocate/0, deallocate/1, stop/0]).


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
    week1 ! {request, self(), allocate},
    receive
      {reply, Reply} -> Reply
    end.

deallocate(Freq) ->
    week1 ! {request, self(), {deallocate, Freq}},
    receive
      {reply, Reply} -> Reply
    end.

stop() ->
    week1 ! {request, self(), stop},
    receive
      {reply, Reply} -> Reply
    end.


% it allows a client to hold more than one frequency; and,
% it allows a client to deallocate a frequency that it is not currently using.

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

    {request, Pid, stop} ->
      Pid ! { reply, stopped }
  end.

start() ->
  ServerPid = spawn(week1, init, []),
  register(week1, ServerPid).

init() ->
  Frequencies = { get_frequencies(), [] },
  loop(Frequencies).

clear() ->
  receive
    _Msg -> clear()
  after 0 ->
    ok
  end.

% tool function
  get_frequencies() -> [10,11,12,13,14,15,16].

test() ->
  Pid = start(),
  week1 ! {request, self(), allocate},
  receive {reply, Reply} -> Reply end.

test_check_and_allocate() ->
  Allocated = [{10,toto}, {12, tata}, {13, titi}],
  Frequencies = {[], Allocated},
  check_and_allocate(Frequencies, tata).

test_check_and_deallocate() ->
  Frequencies = {[], [{10,toto}, {12, tata}, {13, titi}]},
  check_and_deallocate(Frequencies, 12, tata).