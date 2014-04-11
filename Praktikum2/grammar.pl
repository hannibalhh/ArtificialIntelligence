:- consult('familyTree.pl').
:- consult('lex.pl').
:- consult(readsentence).


%%%
% loesche ?;!;.
%%%

%if var is set to string
better_setof(nonvar(Arg),Question,Result):- setof(Arg,Question,Result).
%ansonsten...
better_setof(Arg,Question,Result):- findall(Arg,Question,Res1),list_to_set(Res1,Result).


%%%%%%%%%%%%%%%%%%%%%%%%%
%Test questions
%%%%%%%%%%%%%%%%%%%%%%%%%

find_father(X):- s(X,[wer, ist, ist, die, mutter, von, miriam], []).
ask_sister(X):- s(X,[wer,ist,die,schwester,von,alex], []).
find_brothers(X):- s(X,[wer,ist,der,bruder],[]).
%%%%%%%%%%%%%%%%%%%%%%%%%

ask(Sem) :- read_sentence(List),last(List,'?'),ask(Sem,List,['?']),!.
ask(_) :- writeln('Frag mich doch was!'),fail.

rekursion :- write('Das'),rekursion.
ask([],[was,ist,rekursion,?],['?']) :- rekursion.
ask(Sem,List,End) :-    s(Sem, List,End),!.
ask(_,_,_):- write('Das verstehe ich nicht.').

s(SemS) --> question(SemP,Kind),
	{[_,Arg1,_]=SemP, Question=..SemP, 
	better_setof(Arg1,Question,SemS),write_answer(SemS,SemP,_,Kind),!}.

%Erkennen der Frageart anhand der ersten Wörter.						

%BSP Wer ist die Mutter von Miriam?  Wer=Ipron 
question(SemP,who) 		-->  	ipron(_), who_question(SemWQ),{SemP=SemWQ}.
%BSP Ist Johann der Vater von Alex?
%ist=verb			rest=decision question
question(Sem,dec) 		-->		v(_,_,_,_), dec_question(Sem).				
				
% "ist die mutter von miriam" =verbalphrase (vp)
who_question(SemP) --> 	vp(SemVP),!,
							{([[Pred,[Arg1]]]=SemVP;[[Pred]]=SemVP),SemP=[Pred,_,Arg1],!}.

%entscheidungsfrage besteht aus 2 nominalphrasen
							%Prolog-stuff: semNP2 enthält das verb und das objekt teil 
							%"[vater,_Some_Prolog_var, alex]"
							%SemNP1 enthält das subjekt um das es geht: johann	
							%Die Sem* liegen in komischen klammerungen vor: mit diesem 
							%"oder" holt man nur die bestandteile raus.hinten wird dann neu zusammengebaut.
dec_question(Sem) 	-->		np(SemNP1),np(SemNP2),
							{flatten(SemNP2,SemF),
								([Pred,Arg2]=SemF ; [Pred,_,Arg2]=SemF; [Pred]=SemF), 
							Sem=[Pred,SemNP1,Arg2],!}.

np(Sem)				-->   	name(N,_),{Sem=N}.
np(Sem)				-->		det(_,det,Num,Gen),n(_,SemN,Num,Gen),prepP(SemP),{Sem=[SemN,SemP]}.
np(Sem)				-->		det(_,det,Num,Gen),n(_,SemN,Num,Gen),{Sem=[SemN]}.

prepP(Sem)			-->		prep(_,_,_,_),np(SemNP),{Sem=[SemNP]}. 

vp(Sem)				-->		v(_,_,_,_),np(SemNP),{Sem=[SemNP]}.
vp(_)				-->		v(_,_,_,_).

ipron(X) 				--> [X],{lex(X,_,ipron,_,_)}.
n(N, Sem, Num, Gen)		--> [N],{lex(N,Sem,n,Num,Gen)}.
v(V,Sem,Num,Gen)		--> [V],{lex(_,Sem,v,Num,Gen)}.
det(D,Sem,Num,Gen)		--> [D],{lex(D,Sem,det,Num,Gen)}.
prep(P,Sem,Num,Gen)		--> [P],{lex(P,Sem,prep,Num,Gen)}.
name(N,Gen)				--> [N],{person(N,Gen)}.

write_answer([], Question,N,dec) :- 	write('Nein, es ist '),
										write_answer([], Question,N).
write_answer(R, Question,N,dec) :- 		write('Ja, '),
										write_answer(R, Question,N).									
write_answer([], Question,N,who) :- 	write('Nein, ist keine '),
										write_answer([], Question,N), 
										write(' vorhanden').
write_answer(R, Question,N,who) :- 	    write_answer(R, Question,N).

write_answer([], Question,sg) :- 	
									[Pred,_,_]=Question, 
									lex(Pred,Pred,_,sg,Gen),
									lex(Foo,nein,_,sg,Gen),
									write(Foo),write(' '), write(Pred).
write_answer([X|[]], Question,sg) :- [Pred,_,_]=Question, 
									write(X), lex(Pred,Pred,_,sg,Gen),
									lex(Term,sein,_,sg,Gen),
									write(' '),write(Term),
									write(' '),lex(Det,det,det,sg,Gen),write(Det),write(' '),write(Pred).
write_answer([X,Y], Question,pl) :- [Pred,_,_]=Question, 
									write(X),write(' und '),write(Y), lex(Pred1,Pred,_,pl,Gen),
									lex(Term,sein,_,pl,Gen),
									write(' '),write(Term),
									lex(Art,_,det,pl,_),
									write(' '),write(Art),
									write(' '),write(Pred1).
									
write_answer([X|Rest],Question,_) :- write(X),write(', '),write_answer(Rest,Question,pl).
