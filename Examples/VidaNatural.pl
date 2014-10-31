:-module(_,_).

%patas/2: relacion que es cierta si X es un animal que tiene Y patas
patas(X,8) :-
	  aracnido(X).
patas(X,6) :-
	  insecto(X).
patas(X,0) :-
	  serpiente(X).

%aracnido/1 relacion que es cierta si X es un aracnido
aracnido(tarantula).
aracnido(viuda_negra).
aracnido(alacran).

%insecto/1 relacion que es cierta si X es un insecto
insecto(hormiga).
insecto(mosca).
insecto(avispa).

%serpiente/1 relacion que es cierta si X es una serpiente
serpiente(cobra).
serpiente(vivora).
serpiente(boa).


% objetivos a probar:
%
% ?- patas(X,0).
% ?- patas(boa,X).
% ?- patas(X,8).
% ?- patas(X,N).