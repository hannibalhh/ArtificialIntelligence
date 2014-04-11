% vorgegeben

% Informierte Suche
eval_paths(_,[]).

eval_paths(Strategy,[FirstPath|RestPaths]):-
  eval_path(Strategy,FirstPath),
  eval_paths(Strategy,RestPaths).

insert_new_paths_informed([],OldPaths,OldPaths).

insert_new_paths_informed([FirstNewPath|RestNewPaths],OldPaths,AllPaths):-
  insert_path_informed(FirstNewPath,OldPaths,FirstInserted),
  insert_new_paths_informed(RestNewPaths,FirstInserted,AllPaths).

insert_path_informed(NewPath,[],[NewPath]).

% Wenn der Pfad billiger ist, dann wird er vorn angefügt. (Alte Pfade sind ja sortiert.)
insert_path_informed(NewPath,[FirstPath|RestPaths],[NewPath,FirstPath|RestPaths]):-
  cheaper(NewPath,FirstPath),!.

% Wenn er nicht billiger ist, wird er in den Rest insortiert und der Kopf 
% der Openliste bleibt Kopf der neuen Liste
insert_path_informed(NewPath,[FirstPath|RestPaths],[FirstPath|NewRestPaths]):-
  insert_path_informed(NewPath,RestPaths,NewRestPaths).  

cheaper([(_,_,V1)|_],[(_,_,V2)|_]):-
  V1 =< V2.

bestChild([FirstPath|_RestAllPath], OnlyGoodPath) :-
   %writeln(['first path',FirstPath]),
   isBest(FirstPath),
   !,
   OnlyGoodPath = [FirstPath].

bestChild(_CompleteChildrenPath, OnlyGoodPath) :-
   OnlyGoodPath = [[]].

isBest([(_,_,HeuristikFuerZustand1),(_,_,HeuristikFuerZustand2)|_RestDesWeges]) :-
  HeuristikFuerZustand1 < HeuristikFuerZustand2.