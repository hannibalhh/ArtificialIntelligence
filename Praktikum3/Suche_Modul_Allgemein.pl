% vorgegeben, wir müssen ergänzen:
%	optimistisches Bergsteigen && optimistisches Bergsteigen mit Backtracking (unten)

test :- 
	write(depth),time(solve(depth)),
	write(breadth),time(solve(breadth)),
	write(informed),time(solve(informed)),
	%write(climb),time(solve(climb)),
	write(climbBack),time(solve(climbBack)).

% Das Programm wird mit solve(depth), solve(breadth) oder solve(informed) aufgerufen.
solve(Strategy):-
  start_description(StartState),
  solve((start,StartState,_),Strategy).
  
% Prädikat search: 
%   1. Argument ist die Liste aller Pfade. Der aktuelle Pfad ist an erster Stelle. 
%   Jeder Pfad ist als Liste von Zuständen repräsentiert, allerdings in falscher 
%   Reihenfolge, d.h. der Startzustand ist an letzter Position.
%   2. Argument ist die Strategie
%   3. Argument ist der Ergebnis-Pfad.
%
solve(StartNode,Strategy) :-
  start_node(StartNode),
  search([[StartNode]],Strategy,Path),
  reverse(Path,Path_in_correct_order),
  write_solution(Path_in_correct_order).

write_solution(_).
write_solution(Path):-
  nl,write('SOLUTION:'),nl,
  write_actions(Path).  

write_actions([]).

write_actions([(Action,_,_)|Rest]):-
  write('Action: '),write(Action),nl,
  write_actions(Rest).

% Abbruchbedingung: Wenn ein Zielzustand erreicht ist, wird der aktuelle Pfad an den
% dritten Parameter übertragen.
%
search([[FirstNode|Predecessors]|_],_,[FirstNode|Predecessors]) :- 
  goal_node(FirstNode),
  nl,write('SUCCESS'),nl,!.

search([[FirstNode|Predecessors]|RestPaths],Strategy,Solution) :- 
  expand(FirstNode,Children),                                    % Nachfolge-Zustände berechnen
  generate_new_paths(Children,[FirstNode|Predecessors],NewPaths), % Nachfolge-Zustände einbauen 
  insert_new_paths(Strategy,NewPaths,RestPaths,AllPaths),        % Neue Pfade einsortieren
  search(AllPaths,Strategy,Solution).

generate_new_paths(Children,Path,NewPaths):-
  maplist(get_state,Path,States),
  generate_new_paths_help(Children,Path,States,NewPaths).

% Abbruchbedingung, wenn alle Kindzustände abgearbeitet sind.
%
generate_new_paths_help([],_,_,[]).

% Falls der Kindzustand bereits im Pfad vorhanden war, wird der gesamte Pfad verworfen,
% denn er würde nur in einem Zyklus enden. (Dies betrifft nicht die Fortsetzung des 
% Pfades mit Geschwister-Kindern.) Es wird nicht überprüft, ob der Kindzustand in einem
% anderen Pfad vorkommt, denn möglicherweise ist dieser Weg der günstigere.
%
generate_new_paths_help([FirstChild|RestChildren],Path,States,RestNewPaths):- 
  get_state(FirstChild,State),state_member(State,States),!,
  generate_new_paths_help(RestChildren,Path,States,RestNewPaths).


% Ansonsten, also falls der Kindzustand noch nicht im Pfad vorhanden war, wird er als 
% Nachfolge-Zustand eingebaut.
%
generate_new_paths_help([FirstChild|RestChildren],Path,States,[[FirstChild|Path]|RestNewPaths]):- 
  generate_new_paths_help(RestChildren,Path,States,RestNewPaths).

 
get_state((_,State,_),State).



%%% Strategie:
write_action(_).
write_action([[(Action,_)|_]|_]):-
  nl,write('Action: '),write(Action),nl.

write_next_state([[_,(_,State)|_]|_]):-
  nl,write('Go on with: '),write(State),nl.

write_state([[(_,State)|_]|_]):-
  write('New State: '),write(State),nl.

write_fail(_,_).
write_fail(depth,[[(_,State)|_]|_]):-
  nl,write('FAIL, go on with: '),write(State),nl.


write_fail(_,_):-  nl,write('FAIL').

% Alle Strategien: Keine neuen Pfade vorhanden
insert_new_paths(Strategy,[],OldPaths,OldPaths):-
  write_fail(Strategy,OldPaths),!.

% Tiefensuche
insert_new_paths(depth,NewPaths,OldPaths,AllPaths):-
  append(NewPaths,OldPaths,AllPaths),
  write_action(NewPaths).

% Breitensuche
insert_new_paths(breadth,NewPaths,OldPaths,AllPaths):-
  append(OldPaths,NewPaths,AllPaths).
%  write_next_state(AllPaths),
%  write_action(AllPaths).

% Informierte Suche
insert_new_paths(informed,NewPaths,OldPaths,AllPaths):-
  % eval_paths(informed,NewPaths),
  eval_paths(greedy,NewPaths),
  insert_new_paths_informed(NewPaths,OldPaths,AllPaths).
  %write('oldpath: '),writeln(OldPaths)
%  write_action(AllPaths),
%  write_state(AllPaths).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% optimistisches Bergsteigen && optimistisches Bergsteigen mit Backtracking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% gierige Bestensuche
insert_new_paths(greedy,NewPaths,OldPaths,AllPaths):-
  eval_paths(greedy,NewPaths),
  insert_new_paths_informed(NewPaths,OldPaths,AllPaths).
%  write_action(AllPaths),
%  write_state(AllPaths).

  
insert_new_paths(climb,NewPaths,OldPaths,AllPaths):-
  
  % validiere neue Pfade und entferne alle Pfade die schlechter sind als bisheriger
  eval_paths(climb,NewPaths),
  %eval_paths(greedy,NewPaths),
  insert_new_paths_informed(NewPaths,[],CompleteChildrenPath),
  bestChild(CompleteChildrenPath,AllPaths).
 % writeln(AllPaths),
 % AllPaths = [X|_],
 % X=[(_,_,CurVal),(_,_,NextVal|_],
 % CurVal > NextVal.
 % write(' nextval: '),write(NextVal),
 % write('Old:'),write(OldPaths).
  
 %   OldPaths = [First | _],
 % write('First:'),write(First),
 % First = [(_,_, CurVal)|_],
 % write('CurVal:'),write(CurVal),write('Next:'),writeln(NextVal),
 % isBest([(_,_,NextVal),(_,_, CurVal)]),
 %  writeln('is best').

%  write_action(AllPaths).

% Hill-Climbing backtracking
insert_new_paths(climbBack,NewPaths,OldPaths,AllPaths):-
  %eval_paths(climb,NewPaths),
  eval_paths(greedy,NewPaths),
  insert_new_paths_informed(NewPaths,[],CompleteChildrenPath),
  append(CompleteChildrenPath,OldPaths,AllPaths).
%  write_action(AllPaths),
 % write_state(AllPaths).
