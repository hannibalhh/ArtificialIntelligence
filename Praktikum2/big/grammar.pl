:- module('grammar', [s/1,s/2,s/3, sem/2,sem_convert/2]).
:- consult('Stammbaum.pl').

% Numerus = Singular / Plural.

sem_convert([Pred,[Op,A,B|Rest]|Args], SplitResult) :-
	((Op=';',SplitResult=(X;Result));(Op=',',SplitResult=(X,Result))),
	X =..[Pred,A|Args], sem_convert([Pred, [Op,B|Rest]|Args], Result).
sem_convert([Pred,[Op,A]|Args], Result) :-
	atom(Op),
	sem_convert([Pred,[A]|Args], Result).
sem_convert([Pred,[A]|Args], (X)) :- 
	X =.. [Pred|[A|Args]].

sem(Sem, Satz) :-
	s(S, Satz),
	sem_convert(S,Sem).

s(Satz) :-
    s(_, _, Satz).

s(Semantik, Satz) :-
    s(Semantik, _, Satz).

s(Semantik,ParseTree,Satz) :- 
	s(Semantik,ParseTree,Satz,[]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Bsp.: wer ist der vater von dieter?
s(SemVerbalphrase, parse_tree(ProNominalVerb, Verb)) --> 
	{SemVerbalphrase = [_,[_]|_]},
	pronominaladverbien(ProNominalVerb),
        verbalphrase(SemVerbalphrase, Verb, _, _).

% Bsp.: ist hans der vater von dieter?
s(SemVerbalphrase, parse_tree(Hilfsverb, Nomimal, InterVPN)) -->
	{SemVerbalphrase = [_,_|SemNominalphrase]},
	hilfsverben(Hilfsverb, Numerus), 
        nominalphrase(SemNominalphrase, Nomimal, Numerus, Geschlecht), 
        interrogativerbalphrasepronomen(SemVerbalphrase, InterVPN, Numerus, Geschlecht).

% Bsp.: hans
nominalphrase(Sem,PTree,Numerus,Geschlecht) --> 
	nominalphrase(start,Sem,PTree,Numerus,Geschlecht).

% Bsp.: hans
nominalphrase(_, [SemN], nominalphrase(EigenName), Numerus, Geschlecht) --> 
	eigenname(SemN, EigenName, Numerus, Geschlecht).

% Bsp.: hans und fritzi..
nominalphrase(start, [SemK, SemPN|Semnominalphrase], nominalphrase(EigenName, Konjunktion, NominalPhrase), pl, _) --> 
	eigenname(SemPN, EigenName, _, _), 
        konjunktion(SemK, Konjunktion), 
        nominalphrase(SemK, Semnominalphrase, NominalPhrase, _, _).

% Bsp.: fritzi und dieter..
nominalphrase(SemK, [SemPN|Semnominalphrase], nominalphrase(EigenName, Konjunktion, NominalPhrase), pl, _) --> 
	eigenname(SemPN, EigenName,_,_), 
        konjunktion(SemK, Konjunktion),
        nominalphrase(SemK, Semnominalphrase, NominalPhrase, _, _).

% Bsp.: ist der vater von joe
verbalphrase(SemIverbalphrase, verbalphrase(Verb, InterroPN), Numerus, Geschlecht) --> 
	verb(_SemV, Verb, Numerus), 
        interrogativerbalphrasepronomen(SemIverbalphrase, InterroPN, Numerus, Geschlecht).

%%%
% interrogativerbalphraseronomen == W-Fragen!!! (wer was wo wie warum ..bla)
%%%
% Bsp.: der vater von joe
interrogativerbalphrasepronomen(SemN, interrogativerbalphrasepronomen(Artikel, Nomen, Praepo, ProNomen), Numerus, Geschlecht) --> 
        {SemN = [_,[SemPN],_]},	
        artikel(Artikel, Numerus, Geschlecht,_), 
        nomen(SemN, Nomen, Numerus, Geschlecht,_), 
        praeposition(Praepo, pn), 
        eigenname(SemPN, ProNomen, _Numerus, _).

% Bsp.: der vater
interrogativerbalphrasepronomen(SemN, interrogativerbalphrasepronomen(Artikel, Nomen), Numerus, Geschlecht) --> 
	artikel(Artikel, Numerus, Geschlecht, indef), 
        nomen(SemN, Nomen, Numerus, Geschlecht, indef).

% Bsp.: vaeter (pl!)
interrogativerbalphrasepronomen(SemN, interrogativerbalphrasepronomen(Nomen), pl, Geschlecht) --> 
	nomen(SemN, Nomen, pl, Geschlecht, indef).

% Bsp.: mit || von
interrogativerbalphrasepronomen(SemAdj, interrogativerbalphrasepronomen(Adjektiv), Numerus, _) --> 
	adjektiv(SemAdj, Adjektiv, Numerus, indef).

% Bsp.: mit john verheiratet
interrogativerbalphrasepronomen(SemAdj, interrogativerbalphrasepronomen(Praepo, ProNomen, Adjektiv), Numerus, _) -->
        {SemAdj = [_,[SemPN],_]},	
        praeposition(Praepo, adj),
        eigenname(SemPN, ProNomen, _, _),
        adjektiv(SemAdj, Adjektiv, Numerus, def).

%%%
% Lexikon Zugriff
%%%

% Bsp.: vater,mutter... (Nomen)
nomen(Sem,nomen(X),Numerus,Geschlecht,Def) --> 
        [X], 
        {lex(X, Sem, n, Numerus, Geschlecht, Def)}.

% Bsp.: ist, sind
verb(Sem,verb(X),Numerus) --> 
        [X], 
        {lex(X, Sem, v, Numerus, _)}.

% Bsp.: verheiratet
adjektiv(Sem,adjektiv(X),Numerus,Def) --> 
        [X], 
        {lex(X,Sem,adj,Numerus,_,Def)}.

% Bsp.: peter, björn
% Bsp.: tink, anna
eigenname(X,eigenname(X),sg,Geschlecht) --> 
        [X], 
        {person(X,Geschlecht)}.


% Bsp.: ein, eine / der, die (determiner)
artikel(artikel(X),Numerus,Geschlecht,Def) --> 
        [X], 
        {lex(X,_,artikel,Numerus,Geschlecht,Def)}.

% Bsp.: von
praeposition(praeposition(X),Bind) --> 
        [X], 
        {lex(X,_,praeposition,_,_,Bind)}.

% Bsp.: wer
pronominaladverbien(pronominaladverbien(X)) --> 
        [X], 
        {lex(X,_,pronominaladverbien,_,_)}.

% Bsp.: ist, sind
hilfsverben(hilfsverben(X), N) --> 
        [X], 
        {lex(X,_,hilfsverben, N,_)}.

% Bsp.: und 
konjunktion(Sem, konjunktion(X)) --> 
        [X], 
        {lex(X,Sem,konjunktion,_,_)}.

:- consult('lex.pl').
