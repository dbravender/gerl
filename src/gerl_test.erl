-module(gerl_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

evaluate_test() ->
    ?assert(10 =:= gerl:evaluate(simple_gp, [], {mul, [5, 2]})).

depth_test() ->
    ?assert(3 =:= gerl:depth({mul, [{mul, [10, 20]}, 2]})).

lookup_variable_test() ->
    ?assert(10 =:= gerl:lookup_variable(x, [{x, 10}])).

update_variable_test() ->
    ?assert([{x, 1}, {y, 1}] =:= gerl:update_variable(x, 1, [{x, 0}, {y, 1}])).

generate_test() ->
    random:seed({21015,29080,278}),
    ?assert({add,[{mul,[{y,[]},{y,[]}]},{sub,[{add,[{x,[]},{x,[]}]},{x,[]}]}]} =:= gerl:generate(simple_gp)).

run_cover() ->
    coverize:run(["./src"], gerl_test).
