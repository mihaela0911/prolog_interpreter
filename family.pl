% Факти за родителите
parent(john, mary).
parent(john, mike).
parent(susan, mary).
parent(susan, mike).
parent(mike, tom).
parent(mike, anna).
parent(mary, lisa).

% Пол на индивидите
male(john).
male(mike).
male(tom).
female(susan).
female(mary).
female(anna).
female(lisa).

% Роднински връзки
father(F, C) :- parent(F, C), male(F). 
mother(M, C) :- parent(M, C), female(M). 

sibling(X, Y) :- parent(P, X), parent(P, Y). 
brother(B, S) :- sibling(B, S), male(B).
sister(S, B) :- sibling(S, B), female(S). 

grandparent(GP, GC) :- parent(GP, P), parent(P, GC). 
grandfather(GF, GC) :- grandparent(GF, GC), male(GF). 
grandmother(GM, GC) :- grandparent(GM, GC), female(GM). 

ancestor(A, D) :- parent(A, D). 
ancestor(A, D) :- parent(A, X), ancestor(X, D).

descendant(D, A) :- ancestor(A, D). 

% Братовчеди
cousin(C1, C2) :- parent(P1, C1), parent(P2, C2), sibling(P1, P2).

add(o,X,X).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

test(X,X,Y,Y).