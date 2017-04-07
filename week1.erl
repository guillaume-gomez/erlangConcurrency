-module(week1).
-export([start/0, test/0, init/0, test_check_allocate/0, test_check_deallocate/0]).
-include_lib("eunit/include/eunit.hrl").

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


check_allocate(Frequencies, Pid) ->
  case lists:keysearch(Pid, 2, Frequencies) of
    false -> allocate(Frequencies, Pid);
    _ -> { Frequencies, {error, already_connected}}
  end.


check_deallocate({_Free, Allocated}, Freq, Pid) ->
  ElementFound = lists:keysearch(Freq, 1, Allocated),
  io:format("~w~n", [ElementFound]),
  case validate_deallocate(ElementFound, Pid) of
    true -> {deallocate({_Free, Allocated}, Freq), {ok, Freq}};
    false -> {{_Free, Allocated}, {error, cannot_destroy}}
  end.


validate_deallocate({_, {_Freq, Pid}}, Pid) -> true;

validate_deallocate({_, {_Freq, _PidNode}}, _Pid) -> false;

validate_deallocate(false, Pid) -> false.



loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      { NewFrequencies, Reply } = check_allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);

    {request, Pid, {deallocate, Freq}} ->
      {NewFrequencies, Reply} = check_deallocate(Frequencies, Freq, Pid),
      Pid ! { reply, Reply },
      loop(NewFrequencies);

    {request, Pid, stop} ->
      Pid ! { reply, stopped }
  end.


start() ->
  ServerPid = spawn(week1, init, []).

init() ->
  Frequencies = { get_frequencies(), [] },
  loop(Frequencies).


% tool function
  get_frequencies() -> [10,11,12,13,14,15,16].

test() ->
  Pid = start(),
  register(week1, Pid),
  week1 ! {request, self(), allocate},
  receive {reply, Reply} -> Reply end.

test_check_allocate() ->
  Allocated = [{10,toto}, {12, tata}, {13, titi}],
  check_allocate(Allocated, tata).

test_check_deallocate() ->
  Frequencies = {[], [{10,toto}, {12, tata}, {13, titi}]},
  check_deallocate(Frequencies, 12, tata).