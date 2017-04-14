-module(exceptions).
-export([eval/2,wrap/2]).

% simple model of arithmetical expressions, like
% {add, {num, 0}, {mul, {num,1}, {var,a2}}}

% environment gives value of a variable:
% [ {a,2}, {b,1} ]

eval(_Env,{num,N}) -> N;

eval(Env,{var,V}) ->
    lookup(V,Env);

eval(Env,{mul, E1, E2}) ->
    eval(Env,E1) * eval(Env,E2);

eval(Env,{add, E1, E2}) ->
    eval(Env,E1) + eval(Env,E2);

eval(Env,{'div',Num,Denom}) ->
    N = eval(Env,Num),
    D = eval(Env,Denom),
    case D of
        0 -> throw(div_by_zero);
        _NZ -> N div D
    end.

wrap(Env,Exp) ->
    try eval (Env,Exp) of
        Res -> {ok, Res}
    catch
        throw:div_by_zero -> {error, div_by_zero};
        throw:undefined_variable -> {error, undefined_variable}
    end.

% lookup
% this version assumes it doesn't fail …
% … but could use exceptions to deal with failure (exercise!)

% solution clause
lookup(_X, []) -> throw(undefined_variable);

lookup(X, [{X,N}|_]) -> N;

lookup(X, [_|Rest]) -> lookup(X,Rest).

% examples

%exceptions:eval([ {a,5}, {b,2} ],{add, {var,b}, {var,c}}).
%exceptions:wrap([ {a,5}, {b,2} ],{add, {var,b}, {var,c}}).

