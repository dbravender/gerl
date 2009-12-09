-module(simple_gp).
-behaviour(gerl).
-export([functions/0, terminals/0, fitness/1]).

functions() ->
    [{add, 2, fun(V, [X, Y]) -> gerl:evaluate(?MODULE, V, X) + gerl:evaluate(?MODULE, V, Y) end},
     {sub, 2, fun(V, [X, Y]) -> gerl:evaluate(?MODULE, V, X) - gerl:evaluate(?MODULE, V, Y) end},
     {mul, 2, fun(V, [X, Y]) -> gerl:evaluate(?MODULE, V, X) * gerl:evaluate(?MODULE, V, Y) end},
     {x, 0, fun(V, []) -> gerl:lookup_variable(x, V) end},
     {y, 0, fun(V, []) -> gerl:lookup_variable(y, V) end}].

terminals() ->
    lists:seq(1, 100).

fitness(Organism) ->
    abs(gerl:evaluate(?MODULE, [{x, 10}, {y, 10}], Organism) - 1000 +
        gerl:evaluate(?MODULE, [{x, 20}, {y, 5}], Organism) - 425 +
        gerl:evaluate(?MODULE, [{x, 1}, {y, 1}], Organism) - 1).
