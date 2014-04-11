% person(Name,Gechlecht)
person(johann, m).
person(miriam, w).
person(alex, m).
person(peter, m).
person(julia, w).
person(sarah, w).
person(jana, w).
person(niko, m).
person(jakob, m).
person(vanessa, w).
person(mia, w).
person(ina, w).
person(andreas, m).

% vater(Vater,Kind)
vater(johann, alex).
vater(johann,jakob).
vater(johann,andreas).
vater(alex,sarah).
vater(peter,jana).
vater(peter,ina).
vater(peter,vanessa).
vater(jakob,niko).

% mutter(Mutter,Kind)
mutter(miriam,alex).
mutter(miriam,jakob).
mutter(miriam,andreas).
mutter(jana,sarah).
mutter(julia, jana).
mutter(julia,ina).
mutter(miriam,vanessa).
mutter(mia, niko).

ehemann(johann, miriam).
ehemann(peter, julia).
ehefrau(X,Y):- ehemann(Y,X).

verheiratet(X,Y) :-ehemann(X,Y); ehefrau(X,Y).

% Hilfsdefinition von not
not(P) :- call(P), !, fail.
not(_).

% elternteil(Elternteil,Kind)
elternteil(X,Y):- mutter(X,Y).
elternteil(X,Y):- vater(X,Y).

% bruder(Bruder,VonPerson)
bruder(X,Y) :- person(X,m), vater(Z,X),vater(Z,Y),mutter(U,X),mutter(U,Y),X\=Y.

% schwester(Schwester,VonPerson)
schwester(X,Y) :- person(X,w), vater(Z,X),vater(Z,Y),mutter(U,X),mutter(U,Y),X\=Y.

% brueder(Bruder1,Bruder2)
brueder(X, Y) :- bruder(X,Y),person(Y, m).

% schwestern(Schwester1,Schwester2)
schwestern(X, Y) :- schwester(X, Y),person(Y, w).

% Komplexere Prädikate
% halbschwester(Schwester,VonPerson)
halbschwester(X, Y) :- person(X, w), elternteil(Z,X),elternteil(Z,Y),not(schwester(X,Y)), X\=Y.

% halbbruder(Bruder,VonPerson)
halbbruder(X, Y) :- person(X, m), elternteil(Z,X),elternteil(Z,Y),not(bruder(X,Y)), X\=Y.

% tante(Tante,VonPerson)
tante(X,Y):- elternteil(E,Y), schwester(E,X).

% onkel(Onkel,VonPerson)
onkel(X,Y):- elternteil(E,Y), bruder(E,X).

% cousin(Cousin,VonPerson)
cousin(Cousin,Person) :- person(Cousin,m),onkel(Z,Person),elternteil(Z,Cousin).
cousin(Cousin,Person) :- person(Cousin,m),tante(Z,Person),elternteil(Z,Cousin).

% cousine(Cousine,VonPerson)
cousine(Cousine,Person) :- person(Cousine,w),onkel(Z,Person),elternteil(Z,Cousine).
cousine(Cousine,Person) :- person(Cousine,w),tante(Z,Person),elternteil(Z,Cousine).

% oma,opa, nefe, nichte
oma(X,Y):-mutter(X,Z), elternteil(Z,Y).
opa(X,Y):-vater(X,Z), elternteil(Z,Y).
neffe(X,Y):-(tante(Y,X);onkel(Y,X)),person(X,m).
nichte(X,Y):-(tante(Y,X);onkel(Y,X)),person(X,w).
kind(X,Y):-elternteil(Y,X).
