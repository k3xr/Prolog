%:-module(_,_).

% Caso de la casilla
black.
white.
empty.

% Conexiones en L -> tenemos en cuenta que tenemos el siguiente grafo http://goo.gl/rETg7M
% Estos son todos los movimientos que puede realizar un caballo. Para realizar la comprobación
% de si son posibles estos movimientos creo que será necesario usar un or como: 
% (conexion_L(1,6); conexion(6,1)) cuando queramos saber si podemos 
conexion_L(1,6).
conexion_L(1,8).
conexion_L(2,7).
conexion_L(2,9).
conexion_L(3,4).
conexion_L(3,8).
conexion_L(4,9).
conexion_L(6,7). 

% salto_L_posible/2
% Este predicado es verdadero (tablero 3x3 siempre que el caballo de encuentre en una de las 
% dos posiciones y en su movimiento forme una L.
salto_L_posible(N1, N2):-
	(conexion_L(N1, N2);
	conexion_L(N2, N1)).

% board/9, Ej: board(black, empty, black, empty, empty, empty, white, empty, white)
% Estados InI
board(Pos1, Pos2, Pos3, Pos4, empty, Pos6, Pos7, Pos8, Pos9):-
	Pos1,
	Pos2,
	Pos3,
	Pos4,
	Pos6,
	Pos7,
	Pos8,
	Pos9.
	
% TO-DO
% solve/1, solve(Solution)

solve(Solution):-
	Estado = board(black, empty, black, empty, empty, empty, white, empty, white), %Incilizamos la estructura
	busca_sol(Solution).
	
busca_sol([]).
busca_sol([H|Resto]):-
	arg(1, H, Color),
	arg(2, H, Origin),
	arg(3, H, Destination),
	move(Color, Origin, Destination),
	busca_sol(Resto).


% move(Color, Origin, Destination)
% representa la acción de mover un caballo de color Color (variable que puede tomar uno 
% de los valores constantes black y white) desde la casilla Origin hasta la casilla 
% Destination— que aplicadas secuencialmente desde el estado
% inicial resuelven el problema propuesto

% Ej: de llamada 

move(Color, Origin, Destination):-
	salto_L_posible(Origin, Destination), %Movimientos posibles
	Estado = board(black, empty, black, empty, empty, empty, white, empty, white), %PARA PROBAR move DE FORMA UNITARIA
	Nuevo_Estado = board(_, _, _, _, empty, _, _, _, _),
	arg(Origin, Estado, Color),  %Color en la posicion de origen
	arg(Destination, Estado, empty), %Posicion destino libre
	arg(Origin, Nuevo_Estado, empty), %Origen ahora libre
	arg(Destination, Nuevo_Estado, Color), %Destino ahora con caballo Color
	%Se actualizan el resto de posiciones, a excepcion de donde estaba el caballo y donde esta ahora.
	actualiza_board(9, Origin, Destination, Estado, Nuevo_Estado). 

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


	