-module(week2_exception).
-export([start/0, init/0, clear/0]).
-export([allocate/0, deallocate/1, stop/0, available_frequencies/0, register_frequency_server/1, supervisor_start/0, supervisor_init/0, init_frequency_server/1]).


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
      io:format("the server is overloaded, request failed~n")
    end.

% add a tool function to fetch available frequencies
available_frequencies() ->
  clear(),
  frequency ! {request, self(), server_list},
  receive
      {reply, Reply} -> Reply
  after 1000 ->
    io:format("the server is overloaded, request failed~n")
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
    true ->
      {deallocate({_Free, Allocated}, Freq), {ok, Freq}};
    false ->
      throw(cannot_destroy)
  end.


validate_deallocate({_, {_Freq, Pid}}, Pid) -> true;

validate_deallocate({_, {_Freq, _PidNode}}, _Pid) -> false;

validate_deallocate(false, _Pid) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% I use exception here but it makes the code dirtier, and difficult to read.
%% So in my opinion, using default case as in previous version of check_and_desallocate is better than exception process in message passing system
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      { NewFrequencies, Reply } = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      notify_supervisor(NewFrequencies),
      loop(NewFrequencies);

    {request, Pid, {deallocate, Freq}} ->
      {NewFrequencies, Reply} = inspect(fun() -> check_and_deallocate(Frequencies, Freq, Pid) end, Frequencies),
      Pid ! { reply, Reply },
      notify_supervisor(NewFrequencies),
      loop(NewFrequencies);

    {request, Pid, server_list} ->
      {FreeFrequences, _} = Frequencies,
      Pid ! { reply,  FreeFrequences},
      notify_supervisor(Frequencies),
      loop(Frequencies);

    % catch a possible error
    {'EXIT', Pid, _Reason} ->
      NewFrequencies = exited(Frequencies, Pid),
      notify_supervisor(NewFrequencies),
      loop(NewFrequencies);

    {request, Pid, stop} ->
      Pid ! { reply, stopped };

    {request, Pid, _} ->
      {_, Reply } = inspect(fun() -> throw(unknown_message) end, Frequencies),
      Pid ! Reply,
      loop(Frequencies)
  end.

  inspect(Function, Frequencies) ->
    try Function() of
      {NewFrequencies, Reply} -> {NewFrequencies, Reply}
    catch
      throw:unknown_message -> {Frequencies, {error, unknown_message_bro}};
      throw:cannot_destroy -> {Frequencies, {error, cannot_destroy_motherfucker}}
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
  ServerPid = spawn(?MODULE, init, []),
  register(frequency, ServerPid),
  ServerPid.


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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Supervisor code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

notify_supervisor(Frequencies) ->
  case whereis(supervisor) of
    undefined -> nothing_to_send;
    _ -> supervisor ! {server_notification, whereis(frequency), Frequencies}
  end.

supervisor_start() ->
    register(supervisor,
       spawn(?MODULE,supervisor_init,[])),
    ok.

supervisor_init() ->
  process_flag(trap_exit,true), %Trap exits
  State = { get_frequencies(), [] },
  register_frequency_server(State).

register_frequency_server(State) ->
  ServerPid = spawn_link(?MODULE, init_frequency_server, [State]),
  register(frequency, ServerPid),
  loop_supervisor(ServerPid, State).


loop_supervisor(Pid, State) ->
  receive
    {'EXIT', Pid, _Reason} ->
      register_frequency_server(State);
    {server_notification, Pid, Frequencies} ->
      loop_supervisor(Pid, Frequencies);
    stop ->
      stop()
  end.

init_frequency_server(State) ->
  process_flag(trap_exit, true), % atomic registration
  loop(State).

