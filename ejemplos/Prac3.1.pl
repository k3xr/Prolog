include('encuesta.pl').

equipos_preferidos(L):- 
    %Creamos una lista con todos los equipos de la base de datos 
    findall(X,equipo(X), TodosEquiposAux),
	eliminarRepetidos(TodosEquiposAux,TodosEquipos),
	%Buscamos el elemento mas repetido
	dameElMayor(TodosEquipos, Mayor),
	ocurr(Mayor,N),
	%Buscamos los elementos iguales al Mayor
	dameIguales(TodosEquipos,N,L).

%Para conocer longitud de una lista 
longitud([],0). 
longitud([_|T],N):- 
    longitud(T,N0),  
    N is N0 + 1. 
  
%Numero de ocurrencias 
ocurr(X,N):- 
    findall(X,equipo(X),L),
    longitud(L,N).

%Buscamos los elementos iguales al Mayor
dameIguales([], _, []). 
dameIguales([X|Resto], N, Lista):- 
    equipo(X), 
    ocurr(X,N), 
	dameIguales(Resto,N,ListaAux),
	not(member(X,ListaAux)),
	Lista = [X|ListaAux].	
dameIguales([_|Resto], N, Lista):- 
    dameIguales(Resto, N, Lista).
	
	
%Buscamos el elemento mas repetido en una lista 
dameElMayor([X], X). 
dameElMayor([X|Resto], Mayor):- 
    dameElMayor(Resto, Y), 
    equipo(X), 
    equipo(Y), 
    ocurr(X,NX), 
    ocurr(Y,NY), 
    NX =< NY, 
    Mayor = Y. 
dameElMayor([X|Resto], Mayor):- 
    dameElMayor(Resto, Y), 
    equipo(X), 
    equipo(Y), 
    ocurr(X,NX), 
    ocurr(Y,NY), 
    NX >= NY, 
    Mayor = X. 
	
%JUGADORES

jugadores_no_populares(L):-
    %Creamos una lista con todos los jugadores de la base de datos 
    findall(X,jugador(X), TodosJugadoresAux),
	eliminarRepetidos(TodosJugadoresAux,TodosJugadores),
	%Buscamos el elemento menos repetido
	dameElMenor(TodosJugadores, Menor),
	ocurr_jug(Menor,N),
	%Buscamos los elementos iguales al Menor
	dameIguales_jug(TodosJugadores,N,L).
	
%Numero de ocurrencias jugadores
ocurr_jug(X,N):- 
    findall(X,jugador(X),L),
    longitud(L,N).
	
%Buscamos los elementos iguales al Menor
dameIguales_jug([], _, []). 
dameIguales_jug([X|Resto], N, Lista):- 
    jugador(X), 
    ocurr_jug(X,N), 
	dameIguales_jug(Resto,N,ListaAux),
	not(member(X,ListaAux)),
	Lista = [X|ListaAux].	
dameIguales_jug([_|Resto], N, Lista):- 
    dameIguales_jug(Resto, N, Lista).
	
	
%Buscamos el elemento mas repetido en una lista 
dameElMenor([X], X). 
dameElMenor([X|Resto], Menor):- 
    dameElMenor(Resto, Y), 
    jugador(X), 
    jugador(Y), 
    ocurr_jug(X,NX), 
    ocurr_jug(Y,NY), 
    NX =< NY, 
    Menor = X. 
dameElMenor([X|Resto], Menor):- 
    dameElMenor(Resto, Y), 
    jugador(X), 
    jugador(Y), 
    ocurr_jug(X,NX), 
    ocurr_jug(Y,NY), 
    NY < NX, 
    Menor = Y. 

eliminarRepetidos([],[]).
eliminarRepetidos([X|Resto],Lista):-
	elimina(X,Resto,ListaAux),
	eliminarRepetidos(ListaAux,ListaAux2),
	Lista = [X|ListaAux2].
	
elimina(_, [], []). 
elimina(X,[X|T],R):- 
    elimina(X,T,R). 
elimina(X,[H|T],[H|T1]):-  
    elimina(X,T,T1). 