:-module(_,_).
black.
white.
empty.

isOccupied(white).
isOccupied(black).

%Definición de posiciones posibles.

pos(1).
pos(2).
pos(3).
pos(4).
pos(5).
pos(6).
pos(7).
pos(8).
pos(9).

%Definición de estado casilla
box(black).
box(white).
box(empty).

%definición de tablero:
board(A,B,C,D,E,F,G,H,I):-
box(A),box(B),box(C),box(D),box(E),box(F),box(G),box(H),box(I).

%Definición de movimiento
move(Color,_,_):-
isOccupied(Color).

%Movimientos de un caballo
salto(2,9).
salto(2,7).
salto(3,4).
salto(3,8).
salto(4,9).
salto(4,3).
salto(6,1).
salto(6,7).
salto(7,2).
salto(7,6).
salto(8,3).
salto(8,1).
salto(9,2).
salto(9,4).
salto(1,6).
salto(1,8).

solve(S):-
buscaSol(S,board(black,empty,black,empty,empty,empty,white,empty,white),[]).

buscaSol([],EstadoActual,_):-
EstadoActual = board(white,empty,white,empty,empty,empty,black,empty,black).

buscaSol([Solucion|Resto],EstadoActual,EstadosAnteriores):-
\+member(board(white,empty,white,empty,empty,empty,black,empty,black),EstadosAnteriores),
saltoPosible(EstadoActual,Salto), %No repite
actualizaBoard(Salto ,EstadoActual,EstadoFuturo), %no repite
\+member(EstadoFuturo,EstadosAnteriores), %No parece el error
Solucion=Salto, %Como el salto escogido es bueno, lo damos como solución
buscaSol(Resto,EstadoFuturo,[EstadoActual|EstadosAnteriores]).

%Aqui devolvemos los movimientos posibles
%solo si son legales
saltoPosible(EstadoActual,Salto):-
pos(N),
arg(N,EstadoActual,Box),
isOccupied(Box),
salto(N,F),
arg(F,EstadoActual,empty), %Comprobamos que el movimiento está permitido
Salto = move(Box,N,F).


%Nos devuelve el nuevo estado a que transitamos
actualizaBoard(Salto,EstadoActual, EstadoFuturo):-
arg(2,Salto,Origen), arg(3,Salto,Destino),
functor(EstadoActual, board,9),functor(EstadoFuturo, board,9),
arg(Origen,EstadoActual,Caballo1),arg(Destino,EstadoActual,Espacio1),
arg(Origen,EstadoFuturo,Caballo2),arg(Destino,EstadoFuturo,Espacio2),
Caballo1 = Espacio2, Espacio1 = Caballo2,
rellena(EstadoActual,EstadoFuturo,1). %Necesitamos rellenar las casillas que no cambian


%Este algoritmo rellena los elementos de board que siguen vacios
rellena(_,_,10).

rellena(EstadoActual,EstadoFuturo,N):-
arg(N,EstadoFuturo,Casilla),var(Casilla),
arg(N,EstadoActual,Casilla2),
Casilla=Casilla2,
M is N+1,
rellena(EstadoActual,EstadoFuturo,M).

rellena(EstadoActual,EstadoFuturo,N):-
arg(N,EstadoFuturo,Casilla),nonvar(Casilla),
M is N+1,
rellena(EstadoActual,EstadoFuturo,M).