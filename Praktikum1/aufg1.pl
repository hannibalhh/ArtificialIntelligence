% person(Name,Gechlecht)
person(johann, maennlich).
person(miriam, weiblich).
person(alex, maennlich).
person(peter, maennlich).
person(julia, weiblich).
person(sarah, weiblich).
person(jana, weiblich).
person(niko, maennlich).
person(jakob, maennlich).
person(vanessa, weiblich).
person(mia, weiblich).
person(ina, weiblich).
person(andreas, maennlich).

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

% Hilfsdefinition von not
not(P) :- call(P), !, fail.
not(_).

% elternteil(Elternteil,Kind)
elternteil(X,Y):- mutter(X,Y).
elternteil(X,Y):- vater(X,Y).

% bruder(Bruder,VonPerson)
bruder(X,Y) :- person(X,maennlich), vater(Z,X),vater(Z,Y),mutter(U,X),mutter(U,Y),X\=Y.

% schwester(Schwester,VonPerson)
schwester(X,Y) :- person(X,weiblich), vater(Z,X),vater(Z,Y),mutter(U,X),mutter(U,Y),X\=Y.

% brueder(Bruder1,Bruder2)
brueder(X, Y) :- bruder(X,Y),person(Y, maennlich).

% schwestern(Schwester1,Schwester2)
schwestern(X, Y) :- schwester(X, Y),person(Y, weiblich).

% Komplexere Prädikate
% halbschwester(Schwester,VonPerson)
halbschwester(X, Y) :- person(X, weiblich), elternteil(Z,X),elternteil(Z,Y),not(schwester(X,Y)), X\=Y.

% tante(Tante,VonPerson)
tante(X,Y):- elternteil(E,Y), schwester(E,X).

% onkel(Onkel,VonPerson)
onkel(X,Y):- elternteil(E,Y), bruder(E,X).

% cousin(Cousin,VonPerson)
cousin(Cousin,Person) :- person(Cousin,maennlich),onkel(Z,Person),elternteil(Z,Cousin).
cousin(Cousin,Person) :- person(Cousin,maennlich),tante(Z,Person),elternteil(Z,Cousin).

% cousine(Cousine,VonPerson)
cousine(Cousine,Person) :- person(Cousine,weiblich),onkel(Z,Person),elternteil(Z,Cousine).
cousine(Cousine,Person) :- person(Cousine,weiblich),tante(Z,Person),elternteil(Z,Cousine).