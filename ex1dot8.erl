-module(ex1dot8).
-export([receiver/0, receiver_case/0, receiver_seq/0, test/0]).

receiver() ->
	%timer:sleep(500),
	receive
		stop ->
			io:format("server closed~n");
		{From, Msg} -> 
			io:format("Received '~p' from ~w~n",[Msg,From]), 
			receiver()
	end.


receiver_case() ->
	%timer:sleep(500),
	receive
		Message ->
			case Message of
				stop -> 
					io:format("server_case closed~n");
				{From, Msg} -> 
					io:format("Received_case '~p' from ~w~n",[Msg,From]), 
					receiver_case()
			end
	end.

receiver_seq() ->
	receive
	 	stop -> 
	 		io:format("server closed~n");
	 	{first, Msg} ->
	 		io:format("Received first'~p'~n",[Msg]), 
	 		receiver_seq()
	end,
	receive
		stop -> 
	 		io:format("server closed~n");
	 	{second, Msg2} ->
	 		io:format("Received second '~p'~n",[Msg2]),
	 		receiver_seq()
    end.

client(Pid) ->
	Pid ! {self(), "second"},
	Pid ! {self(), "first"},
	Pid ! {self(), "Lost in the mailbox"},
	Pid ! {self(), "Mais lui"}.

client_seq(Pid) ->
	Pid ! {first, "FirstString"},
	Pid ! {second, "SecondString"}.

client_seq_rev(Pid) ->
	Pid ! {second, "SecondString"},
	Pid ! {first, "FirstString"}.


test() ->
	% io:format("receiver/0~n"),
	% PidReceiver = spawn(ex1dot8, receiver, []),
	% client(PidReceiver),

	% io:format("receiver_case/0~n"),
	% PidReceiver2 = spawn(ex1dot8, receiver_case, []),
	% client(PidReceiver2),


	io:format("server~n"),
	PidReceiver3 = spawn(ex1dot8, receiver_seq, []),
	client_seq(PidReceiver3).

	% io:format("seq_server~n"),
	% PidReceiver3 = spawn(ex1dot8, receiver_seq, []),
	% client_seq(PidReceiver3).

% without timer in receivers
% ex1dot8:test().
% receiver/0
% receiver_case/0
% Received '"second"' from <0.33.0>
% Received '"first"' from <0.33.0>
% Received_case '"second"' from <0.33.0>
% Received '"Lost in the mailbox"' from <0.33.0>
% Received_case '"first"' from <0.33.0>
% {<0.33.0>,"Mais lui"}
% Received '"Mais lui"' from <0.33.0>
% Received_case '"Lost in the mailbox"' from <0.33.0>
% Received_case '"Mais lui"' from <0.33.0>


% with timer
% ex1dot8:test().
% receiver/0
% receiver_case/0
% {<0.33.0>,"Mais lui"}
% Received '"second"' from <0.33.0>
% Received '"second"' from <0.33.0>
% Received '"first"' from <0.33.0>
% Received '"first"' from <0.33.0>
% Received '"Lost in the mailbox"' from <0.33.0>
% Received '"Lost in the mailbox"' from <0.33.0>
% Received '"Mais lui"' from <0.33.0>
% Received '"Mais lui"' from <0.33.0>


% with top level pattern match, receiver/0 will receive any messages
% with case pattern match, receiver_case/0 will only receive message filtered by the case