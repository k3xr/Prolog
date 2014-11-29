:- module(_,_).

prefix([],Ys).
prefix([X|Xs],[X,Ys]) :-
	prefix(Xs,Ys).

suffix(Xs,Xs).
suffix(Xs,[Y|Ys]) :-
	suffix(Xs,Ys).

%suffix([1,2],[1,2,3,1,2]).

sublist(Xs,Ys) :-
	prefix(Xs,Ys).

sublist(Xs,[Y|Ys]) :-
	sublist(Xs,Ys).

sublist_1(Xs,Ys) :- suffix of a prefix
	prefix(Ps,Ys),
	suffix(Xs,Ps).

sublist_2(Xs,Ys) :- prefix of a suffix
	suffix(Ss,Ys),
	prefix(Xs,Ss).

sublist_3(Xs,PsXsSs) :- prefix of a suffix using append 
	append(Ps,XsSs,PsXsSs),
	append(Xs,Ss,XsSs).

sublist_4(Xs,PsXsSs) :- suffix of a prefix using append
	append(PsXs,Ss,PsXsSs),
	append(Ps,Xs,PsXs).

nat(0).
nat(s(N)) :-
	nat(N).

% la longitud de la lista vacía es siempre 0
mylength([],0).

% la longitud de una lista determinada va a ser N+1
% la longitud de la lista menos la cabeza va a ser N
% en cada iteración comprueba si es la lista vacía, 
% y luego si la lista anterior tiene la longitud N
mylength([X|Xs],s(N)) :-
	mylength(xs,N).

% mylength([1,2,3,4,5],N).
% mylength(X, s(s(s(0)))).  -- Devuelve todas las listas con longitud 3: [_,_,_]

myselect([X|Xs],X,Ys). % delete just one occurrence
myselect([Y|Xs],X,[Y|Zs]) :-
	myselect(Xs,X,Zs).

mydelete([X|Xs],X,Ys) :-  %delete all occurrences
	mydelete(Xs,X,Ys).
mydelete([X|Xs],Z,[X|Ys]) :-
	X \= Z,
	mydelete(Xs,Z,Ys).
mydelete([],X,[]).

mypermutation([],[]).
mypermutation(Xs,[X|Zs]) :- 
	myselect(Xs,X,Ys),
	mypermutation(Ys,Zs).

% encaje de patrones
% genera una lista que contiene los mismos elementos 
% en el mismo orden pero dos veces cada elemento
double([],[]).
double([X|Xs],[X,X|Zs]) :-
	double(Xs,Zs).

nomember(X,[]).
nomember(X,[Y|Ys) :-
	X \= Y,
	nomember(X,Ys).

nodobles([],[]).
nodobles([X|Xs],Zs) :-
	member(X,Xs),
	nodobles(Xs,Zs).
nodobles([X|Xs],[X,Zs]) :-
	nomember(X,Xs),
	nodobles(Xs,Zs).

associate([],[],[]).
associate([X|Xs],[Y|Ys],[oficio(X,Y)|Zs]) :-
	associate(Xs,Ys,Zs).

mymember(X,[X|Xs]).
mymember(X,[Y|Ys]) :-
	mymember(X,Ys).

myreverse([],[]).
myreverse([X|Xs],Zs) :-
	myreverse(Xs,Ws),
	append(Ws,[X],Zs).
