%Autores:
%Marco López de Miguel, s100077
%Oscar Fernandez Miguel del Corral, s100048

:-module(_,_).

%(1) Codifique utilizando este formato su propio árbol genealógico, incluyendo al menos los datos del alumno y sus herman@s, padres, tí@s, prim@s y abuel@s.

padres(antonio_padre, carmen, antonio_hijo).
padres(antonio_padre, carmen, loli).
padres(antonio_padre, carmen, silvia).
padres(rafael, jacoba, estrella).
padres(rafael, jacoba, javier_padre).
padres(rafael, jacoba, cesar).
padres(antonio_hijo, estrella, marco).
padres(antonio_hijo, estrella, david).
padres(silvia, vicente, sara).
padres(silvia, vicente, nadia).
padres(loli, santiago, veronica).
padres(loli, santiago, alexis).
padres(loli, santiago, sheila).
padres(cesar, lenka, pablito).
padres(cesar, lenka, hanna).
padres(cesar, lenka, hanka).
padres(javier_padre, maria, javielin).
padres(javier_padre, maria, cesitar).

%(2) Anada una (o varias) reglas que representen el parentesco de hermandad.

hermanos(X,Y):-
		padres(P,M,X),
		padres(P,M,Y),
		X\=Y.

%Anada una regla (o varias) que represente a los primos hermanos.

%Forma simplificada con el uso de OR
primo_hermano2(X,Y):-
		padres(P1,M1,X),
		padres(P2,M2,Y),
		(hermanos(P1,P2); 
		hermanos(P1,M2); 
		hermanos(M1,P2); 
		hermanos(M1,M2)).

%Forma con mutliples reglas
primo_hermano(X,Y):-
		padres(P1,_,X),
		padres(P2,_,Y),
		hermanos(P1,P2).

primo_hermano(X,Y):-
		padres(P1,_,X),
		padres(_,M2,Y),
		hermanos(P1,M2).
		
primo_hermano(X,Y):-
		padres(_,M1,X),
		padres(P2,_,Y),
		hermanos(M1,P2).
		
primo_hermano(X,Y):-
		padres(_,M1,X),
		padres(_,M2,Y),
		hermanos(M1,M2).


%(5) Incluya una regla (o varias, según se juzgue necesario) que represente a los abuelos paternos y maternos (por separado). 
%Se encuentra en la memoria:
%Establezca los objetivos necesarios para responder a las siguientes preguntas: ¿Cuáles son todos los abuelos (incluyendo a los maternos y los paternos) de una persona? 
%¿Cuáles son todos los nietos de cada persona?

%Regla para paternos
paternos(Abuelo,Abuela,X):-
		padres(P,_,X),
		padres(Abuelo,Abuela,P).
		
%Regla para maternos
maternos(Abuelo,Abuela,X):-
		padres(_,M,X),
		padres(Abuelo,Abuela,M).
		
abuelo(X,Abuelo):-
		(paternos(Abuelo,_,X);
		paternos(_,Abuelo,X);
		maternos(Abuelo,_,X);
		maternos(_,Abuelo,X)).
		
%Regla para nietos

nieto(Abu, Nieto):-
		(padres(Abu,_,P);
		padres(_,Abu,P);
		padres(_,Abu,M);
		padres(Abu,_,M)),
		padres(P, M, Nieto).
		
%(6) ¿Cuáles son los ascendientes de una persona? ¿Y los descendientes? (NOTA: utilizar recursividad).

%Ascendientes: verdadero cuando A es descendiente de X
ascendiente(X,A):-
		padres(A,_,X).
		
ascendiente(X,A):-
		padres(_,A,X).
		
ascendiente(X,A):-
		padres(P,_,X),		
		ascendiente(P,A).
		
ascendiente(X,A):-
		padres(_,M,X),		
		ascendiente(M,A).