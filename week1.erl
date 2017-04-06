-module(week1).
-export([]).

allocate({[], Allocated}, _Pid) ->
  {
    {[], Allocated},
    {error, no_frequency}
  };

allocate({[Freq| Free] , Allocated}, _Pid) ->
  {
    {Free, [{Freq, Pid} | Allocated]},
    {ok, Freq}
  };

deallocate({Free, Allocated}, Freq) ->
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq, Free], NewAllocated}.