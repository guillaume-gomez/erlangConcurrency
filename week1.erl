-module(week1).
-export([start/0, test/0, init/0]).

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

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      { NewFrequencies, Reply } = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);

    {request, Pid, {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! { reply, ok },
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