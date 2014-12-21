:-module(_,_).

isOccupied(white).
isOccupied(black).

%Definición de posiciones posibles.
pos(1).
pos(2).
pos(3).
pos(4).
%La posicion 5 no es necesaria puesto que un caballo
%nunca puede ocupar esta posicion.
pos(6).
pos(7).
pos(8).
pos(9).

%Definición de estado casilla
box(black).
box(white).
box(empty).

%Definición de tablero:
board(A,B,C,D,_,F,G,H,I):-
box(A),box(B),box(C),box(D),box(empty),box(F),box(G),box(H),box(I). %La posicion 5 siempre es box(empty)

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

%solve(S)
%Cierto si S es la lista de movimientos que dan como solución el tablero final
solve(S):-
buscaSol(S,board(black,empty,black,empty,empty,empty,white,empty,white),[]).

%buscaSol(L, EstadoActual, EstadosAnteriores)
%Caso de exito, solucion encontrada
buscaSol([],EstadoActual,_):-
EstadoActual = board(white,empty,white,empty,empty,empty,black,empty,black). %Cierto si unifica estado actual con final
																			 %en tal caso solución.

buscaSol([Solucion|Resto],EstadoActual,EstadosAnteriores):-
\+member(board(white,empty,white,empty,empty,empty,black,empty,black),EstadosAnteriores),	%Comprobamos no tenemos estado final
saltoPosible(EstadoActual,Salto), 															%Comprobamos desde EstadoActual es posible salto
actualizaBoard(Salto ,EstadoActual,EstadoFuturo), 											%Actualiza con el salto
\+member(EstadoFuturo,EstadosAnteriores), 													%Comprobamos no hemos estado antes
Solucion=Salto, 																			%Si se cumple lo anterior salto es solucion
buscaSol(Resto,EstadoFuturo,[EstadoActual|EstadosAnteriores]).

%saltoPosible(EstadoActual, Salto)
%Cierto si desde EstadoActual, es posible realizar Salto
%Aqui devolvemos los movimientos posibles
%solo si son legales
saltoPosible(EstadoActual,Salto):-
pos(N),						%Todas las posibles posiciones
arg(N,EstadoActual,Box), 	%Extraemos el Box (contenido) de la casilla N
isOccupied(Box),			%Comprobamos si esta ocupado por blanco/negro
salto(N,F),					%Todos los posibles saltos desde N (Origen) a F (Destino)
arg(F,EstadoActual,empty), 	%Comprobamos que el movimiento está permitido
Salto = move(Box,N,F).		%Obtenemos el movimiento que se realiza

%actualizaBoard(Salto, EstadoActual, EstadoFuturo)
%Cierto si el EstadoFuturo es la aplicacion del Salto al Estado Actual
actualizaBoard(Salto,EstadoActual, EstadoFuturo):-
arg(2,Salto,Origen), arg(3,Salto,Destino),								%Extraemos Origen y Destino del movimiento del caballo
functor(EstadoActual, board,9),functor(EstadoFuturo, board,9),			%EstadoActual y EstadoFuturo son board
arg(Origen,EstadoActual,Caballo1),arg(Destino,EstadoActual,Espacio1),	%Representamos el movimiento en el tablero 
arg(Origen,EstadoFuturo,Caballo2),arg(Destino,EstadoFuturo,Espacio2),	%unificando los terminos
Caballo1 = Espacio2, Espacio1 = Caballo2,
rellena(EstadoActual,EstadoFuturo,1). 									%Mantenemos el resto del estado del tablero


%rellena(EstadoActual, EstadoFuturo)
%Si EstadoFuturo tiene variables libres, unifica estas con las
%variables no libres del EstadoActual
rellena(_,_,10).

rellena(EstadoActual,EstadoFuturo,N):-
arg(N,EstadoFuturo,Casilla),var(Casilla),		%Si Casilla es variable libre
arg(N,EstadoActual,Casilla2),
Casilla=Casilla2,
M is N+1,
rellena(EstadoActual,EstadoFuturo,M).

rellena(EstadoActual,EstadoFuturo,N):-
arg(N,EstadoFuturo,Casilla),nonvar(Casilla),	%Si casilla no es variable libre
M is N+1,
rellena(EstadoActual,EstadoFuturo,M).