%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(gf).
-behaviour(gen_server).

% an implementation of this is included.
-export([start_link/0]).

% you need to implement these functions.
-export([init/1, handle_call/3, handle_cast/2]).

% these are implemented for you.
-export([handle_info/2, terminate/2, code_change/3]).

% you will need to implement these.
-export([allocate/0,deallocate/1,stop/0, inject/1, report/0]).

%% These are the start functions used to create and
%% initialize the server.

start_link() ->
    gen_server:start_link(
		{local, ?MODULE},
		?MODULE, [], []).

init([]) ->
  Frequencies = {get_frequencies(), []},
  {ok, Frequencies}.

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% Functional interface

allocate() ->
  gen_server:call(?MODULE, allocate).

deallocate(Freq) ->
  gen_server:cast(?MODULE, {deallocate, Freq}).

stop() ->
  gen_server:cast(?MODULE, stop).

report() ->
  gen_server:call(?MODULE, report).

inject(List) ->
  gen_server:cast(?MODULE, {inject, List}).

handle_call(allocate, From, Frequencies) ->
  { NewFrequencies, Reply } = allocate(Frequencies, From),
  {reply, Reply, NewFrequencies};

handle_call(report, _From, Frequencies) ->
  {reply, report(Frequencies), Frequencies}.

handle_cast({deallocate, Freq}, Frequencies) ->
  NewFrequencies = deallocate(Frequencies, Freq),
  {noreply, NewFrequencies};

handle_cast({inject, List}, Frequencies) ->
  NewFrequencies = inject(Frequencies, List),
  {noreply, NewFrequencies};

handle_cast(stop, State) ->
  {stop, stopped, State}.




%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

report({FreeFrequencies, _Allocated}) ->
  FreeFrequencies.

inject({Free, Allocated}, Freqs) ->
  {lists:append(Free, Freqs), Allocated}.


% default implementations

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.