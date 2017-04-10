-module(pal_checker_server).
-export([pal_check/1, palindrome_check/1, server/0, server2/1, client/1, multi_server/0, test/0, test_multi_client/0, test_multi_server/0]).

pal_check(String) ->
    String == lists:reverse(String).

rem_punct(String) ->
    lists:filter(fun (Ch) ->
        not(lists:member(Ch,"\"\'\t\n "))
    end,
    String).

to_small(String) ->
    lists:map(fun(Ch) ->
      case ($A =< Ch andalso Ch =< $Z) of
          true -> Ch+32;
          false -> Ch
       end
     end,
     String).

palindrome_check(String) ->
    Normalise = to_small(rem_punct(String)),
    lists:reverse(Normalise) == Normalise.

client(Pid) ->
    Pid ! {check,"Madam I\'m Adam"},
    Pid ! {check,"This is not a palindrome"},
    Pid ! "jkjkjkjkjkjk",
    % stop the server
    Pid ! stop.

client(ServerPid, ClientPid) ->
    ServerPid ! {ClientPid, check,"Madam I\'m Adam"},
    ServerPid ! {ClientPid, check,"This is not a palindrome"}.

display_result(true, Msg) ->
  {result, io:format("\"~s\" is a palindrome~n", [Msg])};

display_result(false, Msg) ->
  {result, io:format("\"~s\" is not a palindrome~n", [Msg])}.

server() ->
    receive
        stop ->
            io:format("finish");
        {check, Msg} ->
            display_result(palindrome_check(Msg), Msg),
            server()
    end.


server2(Pid) ->
    receive
        stop ->
            Pid ! io:format("The server has to stop~n");
        {check, Msg} ->
            Pid ! display_result(palindrome_check(Msg), Msg),
            server2(Pid);
        _ -> Pid ! io:format("The server dont recognize the command~n"),
            server2(Pid)
    end.


multi_server() ->
    receive
        {Pid, stop} ->
            Pid ! io:format("The server has to stop~n");
        {Pid, check, Msg} ->
            Pid ! display_result(palindrome_check(Msg), Msg),
            multi_server();
        _ -> io:format("The server dont recognize the command~n")
    end.


test() ->
  Server = spawn(pal_checker_server, server2, [self()]),
  client(Server).

test_multi_client() ->
    Server = spawn(pal_checker_server, multi_server, []),
    Cl1 = spawn(pal_checker_server, client, [Server]),
    Cl2 = spawn(pal_checker_server, client, [Server]),
    client(Server, Cl1),
    client(Server, Cl2).

test_multi_server() ->
    Server = spawn(pal_checker_server, multi_server, []),
    Server2 = spawn(pal_checker_server, multi_server, []),

    client(Server, self()),
    client(Server2, self()).

