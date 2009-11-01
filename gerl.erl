-module(gerl).
-compile(export_all).

evaluate(Module, Variables, {Key, Args}) ->
    case lists:keysearch(Key, 1, Module:functions()) of
        {value, {_Name, _Arity, Fun}} -> Fun(Variables, Args)
    end;
evaluate(_Module, _Variables, Terminal) -> Terminal.

lookup_variable(VariableName, Variables) -> 
    {value, {_, Value}} = lists:keysearch(VariableName, 1, Variables),
    Value.

random_choice(L) -> 
    lists:nth(random:uniform(length(L)), L).

generate(Module, Depth, MaxDepth) ->
    if Depth > MaxDepth ->
        random_choice(Module:terminals());
    true ->
        Node = random_choice([random_choice(Module:terminals())|Module:functions()]),
        case Node of
            {Name, Arity, _} = Node ->
                if Arity == 0 ->
                    Args = [];
                true ->
                    Args = lists:map(fun(_) -> generate(Module, Depth + 1, MaxDepth) end, lists:seq(1,Arity))
                end,
                {Name, Args};
            Terminal -> Terminal
        end
    end.
generate(Module, MaxDepth) -> generate(Module, 1, MaxDepth).
generate(Module) -> generate(Module, 1, 10).

population(Module, Size) -> population(Module, Size, []).
population(_, 0, Acc) -> Acc;
population(Module, Size, Acc) -> population(Module, Size-1, [generate(Module)|Acc]).

round(Module, Population) ->
    SortedPopulation = lists:map(fun({_, O}) -> O end, lists:keysort(1, lists:map(fun(Organism) -> {Module:fitness(Organism), Organism} end, Population))),
    mate_population(SortedPopulation). 

many_rounds(_, Population, 0) -> Population;
many_rounds(Module, Population, Count) ->
    NewPop = round(Module, Population),
    many_rounds(Module, NewPop, Count -1).

mate_population(Population) -> mate_population(Population, [], length(Population)).
mate_population(_, Acc, 0) -> Acc;
mate_population(Population, Acc, RemainingMates) ->
    [A, B] = mate(select_mate(Population), select_mate(Population)),
    mate_population(Population, [A,B|Acc], RemainingMates - 2).

select_mate(Population) -> select_mate(Population, 25).
select_mate([Organism|[]], _) -> Organism;
select_mate([Organism|Remaining], Chance) ->
    Random = random:uniform(100),
    if 
        Random > Chance -> Organism;
        true -> select_mate(Remaining, Chance)
    end.

random_subtree(Organism) -> random_subtree(Organism, Organism).
random_subtree({_, Args}=Organism, OriginalOrganism) -> 
    Random = random:uniform(10),
    if  
        Args == [] -> Organism;
        Random > 7 -> Organism;
        true -> random_subtree(random_choice(Args), OriginalOrganism)
    end;
random_subtree(Terminal, _) -> Terminal.

depth({_, Args}) -> 1 + lists:max(lists:map(fun depth/1, Args));
depth([]) -> 1;
depth(_) -> 1.

replace({_, []}, Replacement) ->
    Replacement;
replace({Function, Args}, Replacement) ->
    ReplaceHere = random:uniform(10),
    if
        Args == [] orelse ReplaceHere > 7 -> ReplaceWith = fun(_) -> Replacement end;
        true -> ReplaceWith = fun(Arg) -> replace(Arg, Replacement) end
    end,
    Position = random:uniform(length(Args)),
    EvaledReplace = ReplaceWith(lists:nth(Position, Args)),
    {Function, lists:map(fun({_, Value}) -> Value end, lists:keyreplace(Position, 1, lists:zip(lists:seq(1,length(Args)), Args), {Position, EvaledReplace}))};
replace(_, Replacement) -> Replacement.

mate(Organism1, Organism2) ->
    Organism1Gene = random_subtree(Organism1),
    Organism2Gene = random_subtree(Organism2),
    [replace(Organism1, Organism2Gene), replace(Organism2, Organism1Gene)].

test() -> 
    true = 3 =:= depth({mul, [{mul, [10,20]}, 2]}),
    true = 10 =:= lookup_variable(x, [{x,10}]).
