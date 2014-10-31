:-module(_,_).

jefe(luis,jose).
jefe(jose,julia).
jefe(jose,noelia).
jefe(julia,juan).
jefe(juan,alberto).
jefe(juan,pedro).
jefe(noelia,rosa).
jefe(rosa,marcos).
jefe(rosa,david).

curritos(X,Y):-
			jefe(Z,X),
			jefe(Z,Y),
			X\=Y,
			Z\=X.
			
jefazo(X,Y):-
			jefe(X,Y).

jefazo(X,Y):-
			jefe(X,Z),
			jefazo(Z,Y).
