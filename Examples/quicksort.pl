:- module(_,_).

nat(0).
nat(s(N)) :-
	nat(N).

lt(0, s(N)) :-
	nat(s(N)).

lt(s(N), s(M)) :-
	lt(N, M).

gt(s(N), 0) :-
	nat(s(N)).
gt(s(N), s(M)) :-
	gt(N, M).

partition([],_,[],[]).
partition([X|Xs],Pivot,Smalls,[X|Bigs]) :-
	gt(X,Pivot),
	partition(Xs,Pivot,Smalls,Bigs).
partition([X|Xs],Pivot,[X|Smalls],Bigs) :-
	X=Pivot,
	partition(Xs,Pivot,Smalls,Bigs).
partition([X|Xs],Pivot,[X|Smalls],Bigs) :-
	lt(X,Pivot),
	partition(Xs,Pivot,Smalls,Bigs).

quicksort([],[]).
quicksort([X|Xs],SortedXs) :-
	partition(Xs,X,Smalls,Bigs),
	quicksort(Smalls,SortedSmalls),
	quicksort(Bigs,SortedBigs),
	append(SortedSmalls,[X|SortedBigs],SortedXs).

example1(Result) :-
	quicksort([s(s(s(s(0)))),s(s(s(0))),s(s(0)),s(0),0],Result).

example2(Result) :-
	quicksort([s(s(0)),s(s(s(0))),0,s(0),s(s(s(0)))],Result).

