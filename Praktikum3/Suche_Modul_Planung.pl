% vorgegeben, wir mŸssen ergŠnzen:
%       state_member, eval_path

% Die Schnittstelle umfasst
%   start_description   ;Beschreibung des Startzustands
%   start_node          ;Test, ob es sich um einen Startknoten handelt
%   goal_node           ;Test, ob es sich um einen Zielknoten handelt
%   state_member        ;Test, ob eine Zustandsbeschreibung in einer Liste 
%                        von Zustandsbeschreibungen enthalten ist
%   expand              ;Berechnung der Kind-Zustandsbeschreibungen
%   eval-path           ;Bewertung eines Pfades

 start_description([
  block(block1),
  block(block2),
  block(block3),
  block(block4),
  block(block5),
  block(block6),
  on(table,block2),
  on(table,block3),
  on(table,block4),
  
  on(block2,block1),
  on(block1,block5),
  on(block4,block6),
  
  clear(block5),
  clear(block3),
  clear(block6),

  handempty
  ]).

goal_description([
  block(block1),
  block(block2),
  block(block3),
  block(block4),
  block(block5),
  block(block6),
  
  on(block3,block5),
  on(block1,block4),
  on(block4,block2),
  on(block2,block6),
  
  on(table,block3),
  on(table,block1),


  clear(block5),
  clear(block6),
  handempty
  ]).



start_node((start,_,_)).

goal_node((_,State,_)):- goal_description(X), subtract(State,X,[]).
 %"Zielbedingungen einlesen"
  %"Zustand gegen Zielbedingungen testen".

% Aufgrund der Komplexität der Zustandsbeschreibungen kann state_member nicht auf 
% das Standardprädikat member zurückgeführt werden.
%  
state_member(_,[]):- !,fail.

state_member(State,[FirstState|_]):-
  subtract(State,FirstState,Out),[]=Out,!.
 % "Test, ob State bereits durch FirstState beschrieben war. Tipp: Eine
 % Lösungsmöglichkeit besteht in der Verwendung einer Mengenoperation, z.B. subtract"  ,!.

%Es ist sichergestellt, dass die beiden ersten Klauseln nicht zutreffen.
state_member(State,[_|RestStates]):-
           state_member(State, RestStates).
  %"rekursiver Aufruf".

  %nur fŸr informierte suche von bedeutung ansonsten 0
  %eval_state(State,"Rest des Literals bzw. der Klausel","Value berechnen").
eval_path(greedy,[(_,State,Value)|RestPath]):- eval_state(greedy,State,RestPath,Value).
eval_path(informed,[(_,State,Value)|RestPath]):- eval_state(informed,State,RestPath,Value).

eval_path(climb,[(_,State,Value)]):- 
	eval_state( climb, State, _, Value ),writeln(['Value eval_path-short',Value]),!.
eval_path(climb,[(_,State1,Value),(_,State2,Value2)|RestPath]):- 
	eval_state(climb,State1,RestPath,Value),
	eval_state(climb,State2,RestPath,Value2).
	%writeln(['Value eval_path-viel',Value]).
	
% eval fuer Hill Climbing
% vergleiche wieviele conds passen
eval_state(climb,State,_RestPath,Result) :-  
	action(_Name, Cond, _Del, _Add),
	findall((Cond,State), mysubset(Cond,State),All_Actions),
	%writeln(['All_Actions',All_Actions]),
	length(All_Actions,Result).	
	
%eval fuer greedy
eval_state(greedy,State,_RestPath,Value) :-  goal_description(G),subtract(State,G,Tmp),length(Tmp,Value).

% eval fuer A-Algorithmus
eval_state(informed,State,RestPath,Value) :-  
	goal_description(G), 
	subtract(State,G,Tmp),
	length(Tmp,RKosten),
	length(RestPath,BKosten), 
	is(Value, RKosten + BKosten).  

action(pick_up(X),
       [handempty, clear(X), on(table,X)],
       [handempty, clear(X), on(table,X)],
       [holding(X)]).

action(pick_up(X),
       [handempty, clear(X), on(Y,X), block(Y)],
       [handempty, clear(X), on(Y,X)],
       [holding(X), clear(Y)]).

action(put_on_table(X),
       [holding(X)],
       [holding(X)],
       [handempty, clear(X), on(table,X)]).

action(put_on(Y,X),
       [holding(X), clear(Y)],
       [holding(X), clear(Y)],
       [handempty, clear(X), on(Y,X)]).


% Hilfskonstrukt, weil das PROLOG "subset" nicht die Unifikation von Listenelementen 
% durchführt, wenn Variablen enthalten sind. "member" unifiziert hingegen.
%
mysubset([],_).
mysubset([H|T],List):-
  member(H,List),
  mysubset(T,List).


expand_help(State,Name,NewState):- action(Name,Cond, Del,Add)
                                                     ,mysubset(Cond,State)
                                                %     ,writeln(['NCDA State',Name, Cond, Del,Add, State])
                                                     ,subtract(State,Del,NewState1)
                                                 %    ,writeln(['NewState1', Newstate1, 'Del',Del])
                                                     ,append(NewState1,Add,NewState).
  
expand((_,State,_),Result):-
  findall((Name,NewState,_),expand_help(State,Name,NewState),Result).