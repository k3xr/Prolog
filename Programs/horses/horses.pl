:-module(_,_).

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
conexion_L(4,3).
conexion_L(4,9).
conexion_L(6,7). 

% salto_L_posible/2
% Este predicado es verdadero (tablero 3x3 siempre que el caballo de encuentre en una de las 
% dos posiciones y en su movimiento forme una L.
salto_L_posible(N1, N2):-
	(conexion_L(N1, N2);
	conexion_L(N2, N1)).


% Estados 

% TO-DO
% solve/1, solve(Solution)
% move/3, move(Color, Origin, Destination)
% board/9, Ej: board(black, empty, black, empty, empty, empty, white, empty, white)

% move(Color, Origin, Destination)
% representa la acción de mover un caballo de color Color (variable que puede tomar uno 
% de los valores constantes black y white) desde la casilla Origin hasta la casilla 
% Destination— que aplicadas secuencialmente desde el estado
% inicial resuelven el problema propuesto

% Ej: de llamada 

%move(Color, Origin, Destination):-
