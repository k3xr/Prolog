:-module(_,_).


% padres(Padre, Madre, Hijo). 
% padres/3: predicado que es cierto si la persona indicada por el tercer argumento tiene como padre
% la persona indicada por el primer argumento, y tiene como madre la persona indicada por el segundo argumento

padres(jesus, josefina, cesar).
padres(jesus, josefina, mari).
padres(jesus, josefina, jose).
padres(julio, pepi, pepe).
padres(julio, pepi, domingo).
padres(julio, pepi, maite).
padres(julio, pepi, carmen).
padres(cesar, carmen, oscar).
padres(domingo, angela, maria).
padres(domingo, angela, marcos).
padres(manolo, maite, juan).
padres(manolo, maite, daniel).
padres(pepe, maite2, fabio).

% hermanos/2: predicado que es cierto si la persona indicada por el primer argumento y la persona
% indicada por el segundo argumento son hermanos

hermanos(X,Y) :-
		padres(P,M,X),
		padres(P,M,Y),
		X\=Y.


% primos_hermanos/2: predicado que es cierto si la persona indicada por el primer argumento y la persona
% indicada por el segundo argumento son primos hermanos
		
primos_hermanos(X,Y) :-
		padres(P1,_,X),
		padres(P2,_,Y),
		hermanos(P1,P2).
		
primos_hermanos(X,Y) :-
		padres(P1,_,X),
		padres(_,M2,Y),
		hermanos(P1,M2).
		
primos_hermanos(X,Y) :-
		padres(_,M1,X),
		padres(P2,_,Y),
		hermanos(M1,P2).
		
primos_hermanos(X,Y) :-
		padres(_,M1,X),
		padres(_,M2,Y),
		hermanos(M1,M2).

		
% abuelos_maternos/2: predicado que es cierto si la persona indicada por el tercer argumento tiene como abuelo materno
% la persona indicada por el primer argumento, y tiene como abuela materna la persona indicada por el segundo argumento
		
abuelos_maternos(A1,A2,X) :-
		padres(_,M,X),
		padres(A1,A2,M).
		

% abuelos_paternos/2: predicado que es cierto si la persona indicada por el tercer argumento tiene como abuelo paterno
% la persona indicada por el primer argumento, y tiene como abuela paterna la persona indicada por el segundo argumento		
		
abuelos_paternos(A1,A2,X) :-
		padres(P,_,X),
		padres(A1,A2,P).
		

% abuelo/2: predicado que es cierto si la persona indicada por el segundo argumento tiene como abuelo la persona indicada
% por el primer argumento

abuelo(A,X) :-
		abuelos_maternos(A,_,X).
		
abuelo(A,X) :-
		abuelos_maternos(_,A,X).
		
abuelo(A,X) :-
		abuelos_paternos(A,_,X).
		
abuelo(A,X) :-
		abuelos_paternos(_,A,X).
		

% nieto/2: predicado que es cierto si la persona indicada por el segundo argumento tiene como nieto a la persona indicada
% por el primer argumento

nieto(N,X) :-
		abuelo(X,N).
		
% ascendiente/2: predicado que es cierto si la persona indicada por el segundo argumento tiene como ascendiente a la persona indicada
% por el primer argumento

ascendiente(A,X) :-
		padres(A,_,X).
		
ascendiente(A,X) :-
		padres(_,A,X).
		
ascendiente(A,X) :-
		padres(P,_,X),
		ascendiente(A,P).
		
ascendiente(A,X) :-
		padres(_,P,X),
		ascendiente(A,P).
		
