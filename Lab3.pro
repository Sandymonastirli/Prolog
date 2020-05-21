%-- Chrysanthi Monastirli A.M. 1716
%-----------------------------------------------------------------------------------------

%-- ASKHSH 1

q1(C1,C2) :- event(_, C1, D), event(_, C2, D).

q2(X,Y,C) :- at(X,A1,B1,C), at(Y,_,B2,C), dif(X,Y), B2 =< B1, B2 >= A1.
q2(X,Y,C) :- at(X,A1,B1,C), at(Y,A2,_,C), dif(X,Y), A2 =< B1, A2 >= A1.

q3(S) :- event(E, City1, A), event(F, City2, B), event(G, City3, C), dif(E,F), dif(F,G), dif(E,G), country(City1, S), country(City2,S), country(City3,S), B-A<7, C-A<7, A=<B, A=<C.

q4(X) :- at(X,A,B,C1), at(X,K,L,C2), event(_, C1, D1), event(_,C2,D2), D1>=A, D1=<B, D2>=K, D2=<L, country(C1,S1), country(C2,S2), dif(S1,S2).


%-----------------------------------------------------------------------------------------

%-- ASKHSH 2

e(A,B) :- e2(A,B).
e(A,B) :- e2(A,X), e(X,B).

e2(A,B) :- plane(A,B).
e2(A,B) :- boat(A,B).
e2(A,B) :- train(A,B).

q(A,B) :- train(A,B).
q(A,B) :- train(A,X), q(X,B).

q(A,B) :- boat(A,B).
q(A,B) :- boat(A,X), onlytrain(X,B).

q(A,B) :- plane(A,B).
q(A,B) :- plane(A,X), onlytrain(X,B).

onlytrain(A,B) :- train(A,B).
onlytrain(A,B) :- train(A,X), onlytrain(X,B).

p(A,B,N) :- p2(A,X), N>1, p(X,B,N-1).
p(A,B,N) :- p2(A,B), (N =:= 1).
p(A,B,N) :- p3(A,B), (N =:= 0).

p2(A,B) :- plane(A,B).
p2(A,B) :- plane(B,A).
p2(A,B) :- train(A,B).
p2(A,B) :- train(B,A).
p2(A,B) :- boat(A,B).
p2(A,B) :- boat(B,A).

p3(A,B) :- A == B.

%-----------------------------------------------------------------------------------------

%-- ASKHSH 3

thesis(A,B,C,S) :- invalid(A,B,C), S is 0.
thesis(A,B,C,S) :- twoLessThan50(A,B,C), avg(A,B,C,MO), MO>=50, S is 49.
thesis(A,B,C,S) :- twoOver50(A,B,C), avg(A,B,C,MO), MO<50, S is 50.
thesis(A,B,C,S) :- avg(A,B,C,MO), S is MO.

invalid(A,_,_) :- A<0.
invalid(_,B,_) :- B<0.
invalid(_,_,C) :- C<0.
invalid(A,_,_) :- A>100.
invalid(_,B,_) :- B>100.
invalid(_,_,C) :- C>100.

twoLessThan50(A,B,_) :- A<50, B<50.
twoLessThan50(A,_,C) :- A<50, C<50.
twoLessThan50(_,B,C) :- B<50, C<50.

avg(A,B,C,MO) :- A=<B, B=<C, MO is ((35*A+40*B+50*C)//125).
avg(A,B,C,MO) :- A=<C, C=<B, MO is ((35*A+40*C+50*B)//125).
avg(A,B,C,MO) :- B=<A, A=<C, MO is ((35*B+40*A+50*C)//125).
avg(A,B,C,MO) :- B=<C, C=<A, MO is ((35*B+40*C+50*A)//125).
avg(A,B,C,MO) :- C=<A, A=<B, MO is ((35*C+40*A+50*B)//125).
avg(A,B,C,MO) :- C=<B, B=<A, MO is ((35*C+40*B+50*A)//125).

twoOver50(A,B,_) :- A>=50, B>=50.
twoOver50(A,_,C) :- A>=50, C>=50.
twoOver50(_,B,C) :- C>=50, B>=50.


%-----------------------------------------------------------------------------------------

%-- ASKHSH 4

d(_,K,_,D) :- K=:=0, D is 0.
d(_,K,N,D) :- K=:=1, D is N.
d(I,K,N,D) :- 2=<K, K=<I, d(I mod K,K,N,D).
d(I,K,N,D) :- K=:=2, I=:=0, f(0,0,N,F), D is (N-(F*(F+1))//2).
d(I,K,N,D) :- K=:=2, I=:=1, f(0,0,N,F), d(0,2,N,D2), D is (F - D2).
d(I,K,N,D) :- K>=3, I=:=0, d(0,2,N,D).
d(I,K,N,D) :- K>=3, 1=<I, I=<K, d(1,2,N,D2), d(I-1,K-1,D2,D).

f(X,Y,N,F) :- Y>N, F is (X-1).
f(X,Y,N,F) :- f(X+1,Y+X+1,N, F).


%-----------------------------------------------------------------------------------------

%-- ASKHSH 5

oplus(0,0,0).


oplus(r(X),r(Y),r(Z)) :- oplus(X,Y,Z).
oplus(r(X),s(Y),r(Z)) :- oplus(X,s(Y),Z).
oplus(r(X),0,r(Z)) :- 	oplus(X,0,Z).
oplus(s(X),r(Y),r(Z)) :- oplus(s(X),Y,Z).
oplus(0,r(Y),r(Z)) :- 	oplus(0,Y,Z).

oplus(s(X),s(Y),Z) :- 		oplus2(X,Y,Z).
oplus(s(X),0,s(Z)) :- 		oplus2(X,0,Z).
oplus(0,s(Y),s(Z)) :- 		oplus2(0,Y,Z).

oplus2(s(X),s(Y),Z) :- 		oplus2(X,Y,Z).
oplus2(s(X),0,s(Z)) :- 		oplus2(X,0,Z).
oplus2(0,s(Y),s(Z)) :- 		oplus2(0,Y,Z).

oplus2(0,0,0).

%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

%-- MHN TROPOPOIHSETE TO PARAKATW TMHMA KWDIKA 


dif(X,Y) :- X \= Y.


at(tiger,1,14,'Berlin').
at(tiger,15,15,'Hamburg').
at(tiger,16,37,'Athens').
at(tiger,38,50,'Rome').
at(wolf,1,4,'Rome').
at(wolf,5,5,'London').
at(wolf,6,7,'Rome').
at(wolf,8,8,'Rotterdam').
at(wolf,9,24,'Rome').
at(wolf,25,25,'Athens').
at(wolf,26,30,'Rome').
at(wolf,31,40,'Paris').
at(wolf,41,41,'Rome').
at(wolf,42,42,'Barcelona').
at(wolf,43,50,'Rome').
at(hawk,1,10,'Berlin').
at(hawk,11,20,'Stuttgart').
at(hawk,21,35,'Hamburg').
at(hawk,36,50,'Frankfurt').
at(shark,1,16,'Amsterdam').
at(shark,17,20,'London').
at(shark,21,29,'Paris').
at(shark,30,45,'Rome').
at(shark,43,48,'Brussels').
at(shark,49,50,'London').
at(spider,1,12,'Brussels').
at(spider,13,17,'Berlin').
at(spider,18,50,'Brussels').
at(snake,1,10,'Rome').
at(snake,11,20,'Milan').
at(snake,21,50,'Berlin').




event(e001,'Berlin',2).
event(e002,'Madrid',3).
event(e003,'London',5).
event(e004,'Rome',7).
event(e005,'Bristol',10).
event(e006,'Stuttgart',13).
event(e007,'Milan',17).
event(e008,'Amsterdam',17).
event(e009,'Rotterdam',18).
event(e010,'Hamburg',24).
event(e011,'Amsterdam',24).
event(e012,'Athens',25).
event(e013,'Groningen',25).
event(e014,'Paris',31).
event(e015,'Strasbourg',31).
event(e016,'Paris',37).
event(e017,'Brussels',40).
event(e018,'Brussels',41).
event(e019,'Barcelona',42).
event(e020,'Frankfurt',43).
event(e021,'Brussels',43).
event(e022,'London',47).



country('Amsterdam','Netherlands').
country('Athens','Greece').
country('Barcelona','Spain').
country('Berlin','Germany').
country('Bristol','United Kingdom').
country('Brussels','Belgium').
country('Frankfurt','Germany').
country('Groningen','Netherlands').
country('Hamburg','Germany').
country('London','United Kingdom').
country('Madrid','Spain').
country('Milan','Italy').
country('Paris','France').
country('Rome','Italy').
country('Rotterdam','Netherlands').
country('Strasbourg','France').
country('Stuttgart','Germany').




train(2,3).
train(3,4).
train(5,6).
train(6,7).
train(7,8).
train(7,9).
train(10,11).
train(10,13).
train(11,12).
train(12,13).
train(14,15).

boat(1,4).
boat(4,5).
boat(5,9).
boat(9,10).
boat(12,15).

plane(1,3).
plane(1,6).
plane(3,11).
plane(6,9).
plane(6,11).
plane(6,16).
plane(15,16).


