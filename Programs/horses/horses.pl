:-module(_,_).

% Caso de la casilla
black.
white.
empty.

isColor(black).
isColor(white).
% Conexiones en L -> tenemos en cuenta que tenemos el siguiente grafo http://goo.gl/rETg7M
% Movimientos que puede realizar un caballo.
conexion_L(1,6).
conexion_L(1,8).
conexion_L(2,7).
conexion_L(2,9).
conexion_L(3,4).
conexion_L(3,8).
conexion_L(4,9).
conexion_L(6,7). 

% salto_L_posible/2
% Este predicado es verdadero (tablero 3x3) siempre que el caballo se encuentre en una de las 
% dos posiciones y en su movimiento forme una L.
salto_L_posible(N1, N2):-
	(conexion_L(N1, N2);
	conexion_L(N2, N1)).

% board/9, Ej: board(black, empty, black, empty, empty, empty, white, empty, white)
board(Pos1, Pos2, Pos3, Pos4, empty, Pos6, Pos7, Pos8, Pos9):-
	Pos1,
	Pos2,
	Pos3,
	Pos4,
	Pos6,
	Pos7,
	Pos8,
	Pos9.

solve(Solution):-
	busca_sol(Solution,board(black, empty, black, empty, empty, empty, white, empty, white),[]).

% Condición de parada.	
busca_sol([],EstadoActual,_):-
	EstadoActual = board(white, empty, white, empty, empty, empty, black, empty, black),!.
	
busca_sol([H|Resto],EstadoActual,EstadosAnteriores):-
	salto_L_posible(Origin, Destination), %Movimientos posibles
	H=move(Color,Origin,Destination), %H es una estructura move/3
	arg(Origin, EstadoActual, Color),  %Color en la posicion de origen
	isColor(Color), %Comprobamos que lo que hemos cogido es un caballo
	arg(Destination, EstadoActual, empty), %Posicion destino libre
	Nuevo_Estado = board(_, _, _, _, empty, _, _, _, _), %Nuevo_Estado es una estructura board
	arg(Origin, Nuevo_Estado, empty), %Origen ahora libre
	arg(Destination, Nuevo_Estado, Color), %Destino ahora con caballo Color
	%Se actualizan el resto de posiciones, a excepcion de donde estaba el caballo y donde esta ahora.
	actualiza_board(9, Origin, Destination, EstadoActual, Nuevo_Estado),
	%Comprobamos que no hayamos estado en el estado al que pretendemos transitar
	\+member(Nuevo_Estado,EstadosAnteriores),
	append(EstadosAnteriores,[EstadoActual],NuevosEstadosAnteriores),
	busca_sol(Resto,Nuevo_Estado,NuevosEstadosAnteriores).

% move(Color, Origin, Destination)
% representa la acción de mover un caballo
move(Color, Origin, Destination):-
	Color,
	Origin,
	Destination.

actualiza_board(0, _, _, _, _).
actualiza_board(N, Origen, Destino, Board, Nuevo_Board) :- 
	(N = Origen;
	N = Destino),
	M is N-1, actualiza_board(M, Origen, Destino, Board, Nuevo_Board).
actualiza_board(N, Origen, Destino, Board, Nuevo_Board) :- 
	N>0,
	N \== Origen,
	N \== Destino,
	arg(N, Board, Pos1),
	arg(N, Nuevo_Board, Pos1),
	M is N-1, 
	actualiza_board(M, Origen, Destino, Board, Nuevo_Board).
	