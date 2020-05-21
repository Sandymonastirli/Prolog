% Monastirli Chrysanthi AM 1716 --- 
%-----------------------------------------------------------------------------------------
use_module(library(lists)).
%-- ASKHSH 1

p(X,A,N) :- p1(X,A,1,0,N).

p1(X,A,I,L,K) :- L + 1-(A/I) >=X, K is I.
p1(X,A,I,L,K) :- p1(X,A,(I+1),(L + 1-(A/I)),K).

%-----------------------------------------------------------------------------------------

%-- ASKHSH 2
sumOf2Cubes(N) :- K is floor(N^(1/2)), between(1, K, X), between(1, K, Y), X3 is X^3, Y3 is Y^3, N is X3+Y3.

%-----------------------------------------------------------------------------------------

%-- ASKHSH 3
replace(L,X,S,R) :- length(L,L1), L1 =:= 0, R = [].
replace(L,X,S,R) :- [H|T] = L, H == X, length(S, L1), L1==0, replace(T,X,[],R2), R = R2.
replace(L,X,S,R) :- [H|T] = L, H == X, [I|J] = S, replace(T,X,J,R2), R = [I|R2].
replace(L,X,S,R) :- [H|T] = L, replace(T,X,S,R2), R = [H|R2].
%-----------------------------------------------------------------------------------------

%-- ASKHSH 4


majority(L,X) :- majority2(L, 0, X).

majority2(L, I, X) :- nth0(I, L, E), countSum(E, L, C), length(L, L1), C*2>L1,  X = E.
majority2(L, I, X) :- I2 is (I+1), length(L, L1), I<L1, majority2(L, I2, X).

countSum(X, L, C) :- length(L,L1), L1=:=0, C is 0 .
countSum(X, L, C) :- [H|T] = L,  X == H, countSum(X, T, C2), C is (C2 + 1).
countSum(X, L, C) :- [H|T] = L, countSum(X, T, C).
%-----------------------------------------------------------------------------------------

%-- ASKHSH 5
domino(L) :- length(L,L1), L1 == 1.
domino(L) :- select(X, L, L1), select(Y, L1, L2), lastElem(X, E1), firstElem(Y, E1), domino2(L2, Y).

domino2(L, X) :- length(L, L1), L1 == 0.
domino2(L, X) :- select(Y, L, L1), lastElem(X, E1), firstElem(Y, E1), domino2(L1, Y).

lastElem(L, E) :- length(L, L1), L1>0, last(L, E).

firstElem(L, E) :- length(L, L1), L1>0, [H|T] = L, E = H.