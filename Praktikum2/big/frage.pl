:- use_module('grammar').
:- consult('readsentence.pl').

%%%
% loesche ?;!;.
%%%

cleanup_sentence(Input, Output) :-
	select('?', Input, Output);
	select('.', Input, Output);
	select('!', Input, Output).
cleanup_sentence(X, X).

%%%
% fuege grammer: hinzu
%%%

add_namespace(Namespace, Input, Output) :-
	Input =.. [_Predicate | _Args],
	Output =.. [:, Namespace, Input].

%%%
% auswertung
%%%

turn([X,Y,Z],[X,Z,Y]).

evaluate(S, yes, AnswerSentences) :-
	%sem_convert(S,Sem),
	flatten(S,SL),
	turn(SL,SL2),
	Sem =.. SL2,
        add_namespace(grammar, Sem, NewSem),
	term_variables(NewSem, Vars),
        setof(Vars, NewSem, Results),
        flatten(Results, AnswerSentences).       


evaluate(_Sem, no, [keinen]).

%%%
% fragen
%%%

f :-  
        write('Was kann ich für Sie tun?: '),
        read_sentence(Sentence),
        cleanup_sentence(Sentence, CleanSentence),
        fCheck(CleanSentence),!.
        
f1 :-  
        cleanup_sentence([ist,ute,die,mutter,von,franz],CleanSentence),
        fCheck(CleanSentence).
f2 :-  
        cleanup_sentence([ist,miriam,die,mutter,von,alex],CleanSentence),
        fCheck(CleanSentence).
f3 :-  
        cleanup_sentence([wer,ist,der,cousin,von,sarah],CleanSentence),
        fCheck(CleanSentence).
f4 :-  
        cleanup_sentence([wer,ist,die,cousine,von,niko],CleanSentence),
        fCheck(CleanSentence).        
        
% funktioniert        
% Wer ist die Ehefrau von Peter?
% Wer ist der Ehemann von Miriam?
% Ist Johann der Vater von Alex?
% Ist Miriam die Mutter von Alex?
% Ist Vanessa die Halbschwester von Jana?
% Ist Alex der Halbbruder von Vanessa?
% Ist Ina die Schwester von Jana?
% Wer sind die Schwestern von Jana?
% Wer ist der Bruder von Alex?
% Ist Jakob der Onkel von Sarah?
% Wer ist die Tante von Sarah?
% Ist Sarah die Nichte von Ina?
% Wer ist der Neffe von Alex?
% Ist Johann der Opa von Sarah?
% Wer ist die Oma von Sarah?
% Wer ist die Nichte von Ina?
% Elternteil?

% funktioniert nicht
% Ist Miriam verheiratet?

printAnswer([]) :- writeln('Ja, das ist korrekt.').
printAnswer([H|T]) :- write('Ich kenne '),write(H),andPrint(T).
andPrint([]) :- write('.').
andPrint([H]) :- write(' und '),write(H).
andPrint([H|T]) :- write(', '),write(H),andPrint(T).
     
fCheck(Sentence) :-  
        s(S, Sentence),!,
        evaluate(S, _Success, Answers),!,
        printAnswer(Answers).
        

fCheck(Sentence) :-
        Sentence == [was, ist, die, antwort, auf, alle, fragen],!,
        writeln(42).

fCheck(Sentence) :-
        Sentence == [wer, sind, die, onkelz],!,
        writeln('Was ist das denn für ne Frage!?').

fCheck(Sentence) :-
        Sentence = [wer, sind, die, onkels | _],!,
        writeln('Du kennst die Onkelz nicht!?').

fCheck(Sentence) :-
        Sentence == [was, ist, rekursion],!,
        writeln('Das ist Rekursion'),fCheck(Sentence).

fCheck(_) :- writeln('Sorry, versteh ich nicht.').
