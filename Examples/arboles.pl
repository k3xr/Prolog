%    Clase 23 10 2014   %
%%%%%%%%%%%%%%%%%%%%%%%%%

%Arboles binarios

% Nombre: Enrique Ortiz Pasamontes
% Matrícula: S100110

:- module(_,_).

%Preorder, si representamos el nodo primero [nodo,izda,drcha]
pre_order(void,[]).
pre_order(tree(X,Left,Right),Order):-
	pre_order(Left,OrderLeft),
	pre_order(Right,OrderRight),
	append([X|OrderLeft],OrderRight,Order).

%Postorder
%Inorder



%%POLIMORFISMO

%Predicados únicospara distintos tipos de datos:

lt_member(X,[X|Y]):- list(Y).
lt_member(X,tree(X,L,R)):- loquesea.

%En este caso tenemos el mismo funtor representando ambos predicados de aridad
% 2, pero cambian los tipos de elementos con los que operamos.


%%Expresiones simbólicas.
  % Polinomios Transparencia 23
  % Diferenciación simbólica. Derivadas. Transparencia 24 falta decir que X debe de ser un término



  