-module(pal_checker_server).
-export([pal_check/1, palindrome_check/1, server/0, server2/1, client/1, test/0]).

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


test() ->
  Server = spawn(pal_checker_server, server2, [self()]),
  pal_checker_server:client(Server).