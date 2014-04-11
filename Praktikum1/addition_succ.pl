add(0,Y,Y).

add(succ(X),Y,f(Z)) :-
  add(X,Y,Z).
