-module(gerl).
-compile(export_all).

evaluate(Module, Variables, {Key, Args}) ->
    case lists:keysearch(Key, 1, Module:functions()) of
        {value, {_Name, _Arity, Fun}} -> Fun(Variables, Args)
    end;
evaluate(_Module, _Variables, Terminal) ->
     Terminal.

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

tournament(Module, Population, BracketSize, Parameters) ->
    tournament(Module, [Population|[]], BracketSize, Parameters, false).
tournament(Module, [Population|PreviousLosers], BracketSize, Parameters, false) ->
    GameBrackets = ks_utility:divide_list(Population,BracketSize),
    FilterWinner = fun(Bracket) -> 
        Scores = Module:tournament_fitness(Bracket, Parameters),
        lists:nth(1,lists:keysort(1,[{lists:nth(N,Scores),lists:nth(N,Bracket)} || N <- lists:seq(1,BracketSize)]))
    end,
    WinnersWithScores = lists:map(FilterWinner, GameBrackets),
    
    Winners = lists:map(
        fun({_, Organism}) -> Organism end,
        WinnersWithScores
    ),


    NewLosers = Population -- Winners,
    tournament(Module, [Winners,NewLosers|PreviousLosers], BracketSize, Parameters, length(Winners) == BracketSize);
tournament(Module, Population, _, _, true) ->
%    SortedPopulation = lists:flatten(Population),
    {unfinished,Module,Population}.
    

update_variable(Var, NewVal, Variables) ->
    lists:keyreplace(Var, 1, Variables, {Var, NewVal}).

receive_loop([],Results) -> Results;
receive_loop(InboundNodes,Results) ->
    receive
        {fitness_result,Node,Result} -> receive_loop(InboundNodes -- [Node],[Result|Results])
    end.

partition_workload(Population,Nodes) ->
    P = length(Population),
    N = length(Nodes),
    StdSize = P div N,
    partition_workload(Population,Nodes,StdSize,[]).

%% Tack the last little bit on the end
partition_workload(Population,[],_StdSize,[{Node,SubPop}|Partitions]) -> [{Node,SubPop ++ Population}|Partitions];
partition_workload(Population,[Node|Nodes],StdSize,Partitions) ->
    SubPop = lists:sublist(Population,StdSize),
    Partition = {Node,SubPop},
    partition_workload(Population -- SubPop,Nodes,StdSize,[Partition|Partitions]).

distributed_round(Sender,Module,SubPopulation,Parameters) ->
    io:format("About to tell ~w where he can stick it!",[Sender]),
    Sender ! {fitness_result,self(),lists:map(fun(Organism) -> {Module:fitness(Organism, Parameters), Organism} end, SubPopulation)}.

distribute_rounds(Module,Population,Parameters) ->
    Nodes = nodes(),
    Partitions = partition_workload(Population,Nodes), %% Each elem is {Node,SubPop}
    Pids = lists:foldl(
        fun({Node,SubPop},Acc) ->
            Pid = spawn(Node,gerl,distributed_round,[self(),Module,SubPop,Parameters]),
            io:format("Just spawned ~w organisms all over ~w",[length(SubPop),Pid]),
            [Pid|Acc]
        end,
        [],Partitions
    ),
    Results = lists:flatten(receive_loop(Pids,[])),
    {SortedPopulation,Report} = generate_report(Results),
    {mate_population(Module,SortedPopulation), Report}.
    
    %% Need to spawn and then go into server mode until they all return

round(Module, Population, Parameters) ->
    PopulationWithScores = lists:map(
        fun(Organism) -> {Module:fitness(Organism, Parameters), Organism} end,
        Population
    ),
    {SortedPopulation,Report} = generate_report(PopulationWithScores),
    {mate_population(Module,SortedPopulation), Report}. 

generate_report(Population) ->
    Length = length(Population),
    SortedPopulationWithScores = lists:reverse(lists:keysort(1, Population)),
    SortedPopulation = lists:map(
        fun({_, Organism}) -> Organism end,
        SortedPopulationWithScores
    ),
    Scores = lists:map(
        fun({Score, _}) -> Score end,
        SortedPopulationWithScores
    ),
    BestScore = lists:nth(1,Scores),
    WorstScore = lists:nth(Length, Scores),
    AverageScore = lists:sum(Scores) / Length,
    Deviations = lists:map(
        fun(Score) -> abs(Score - AverageScore) end,
        Scores
    ),
    StandardDeviation = lists:sum(Deviations) / Length,
    {SortedPopulation,[{bst,BestScore},{wst,WorstScore},{avg,AverageScore},{std,StandardDeviation}]}.

round_until(Module, Population, Condition, Parameters) -> round_until(Module, Population, Condition, Parameters, true).
round_until(Module, Population, Condition, Parameters, true) ->
    {CPUTime,{NewPop, Report}} = timer:tc(gerl,round,[Module, Population, Parameters]),
    FullReport = [{time,CPUTime}|Report],
    io:write(FullReport),
    io:nl(),
    Test = Condition(Report),
    round_until(Module, NewPop, Condition, Parameters, Test);
round_until(_, Population, _, _, false) -> 
    %% Print out the best organism
    io:fwrite("Terminating GP Experiment with:~n"),
    io:write(lists:nth(1,Population)).

many_rounds(_, Population, _, 0) -> Population;
many_rounds(Module, Population, Parameters, Count) ->
    {NewPop,_} = round(Module, Population, Parameters),
    many_rounds(Module, NewPop, Parameters, Count - 1).

mate_population(Module, Population) -> mate_population(Module, Population, [], length(Population)).
mate_population(_, _, Acc, 0) -> Acc;
mate_population(Module, Population, Acc, RemainingMates) ->
    [A, B] = mate(Module, select_mate(Population), select_mate(Population)),
    mate_population(Module, Population, [A,B|Acc], RemainingMates - 2).

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

mutate(Module, X) -> replace(X,generate(Module, 3)).

mate(Module, Organism1, Organism2) ->
    Organism1Gene = random_subtree(Organism1),
    Organism2Gene = random_subtree(Organism2),
    [mutate(Module, replace(Organism1, Organism2Gene)), mutate(Module, replace(Organism2, Organism1Gene))].

behaviour_info(callbacks) ->
    [{functions, 0},
     {terminals, 0},
     {fitness,   1}];

behaviour_info(_) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Experiments API
%% To be called from erl prompt
%%%%%%%%%%%%%%%%%%%%%%%%%%%
reseed() ->
    {A,B,C} = now(),
    random:seed(A,B,C).
    
std_tournament(Module, PopSize, Threshold, Parameters) ->
    Condition = fun([{bst,BestScore},{wst,_},{avg,_},{std,_}]) -> BestScore > Threshold end,
    Pop = population(Module, PopSize),
    tournament(Module, Pop, Condition, Parameters).

std_experiment(Module, PopSize, Threshold, Parameters) ->
    Condition = fun([{bst,BestScore},{wst,_},{avg,_},{std,_}]) -> BestScore > Threshold end,
    custom_experiment(Module, PopSize, Condition, Parameters).

reverse_experiment(Module, PopSize, Threshold, Parameters) ->
    Condition = fun([{bst,BestScore},{wst,_},{avg,_},{std,_}]) -> BestScore < Threshold end,
    custom_experiment(Module, PopSize, Condition, Parameters).

custom_experiment(Module, PopSize, Condition, Parameters) ->
    _ = reseed(),
    Pop = population(Module, PopSize),
    round_until(Module, Pop, Condition, Parameters).

