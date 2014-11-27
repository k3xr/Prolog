%    Ejercicoi clase 16 10 2014   %
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Nombre: Enrique Ortiz Pasamontes
% Matrícula: S100110

:- module(_,_).

% Empleamos recursividad para poder obtener los prefijos sucesivos de una lista.
%Especificamos caso base, la lista vacia es el prefijo simpre de cualquier otra lista.
prefix([],Ys). 

%Si tenemos un elemento cabeza de lista X y el resto de la lista Xs y otra lista
%Ys con primera cabeza X. Verificamos que las dos listas contienen la misma cabeza de lista
%Y que además Xs es todo un prefijo de Ys: prefix([1,2,3],[1,2,3,4,5,6]).
prefix([X|Xs],[X|Ys]):-  
	prefix(Xs,Ys). 

%Cualquier lista es sufijo de ella misma
%Una lista Xs es una lista sufijo de Ys si Xs es sufijo con primer elemento de Ys
sufix(Xs,Xs).
sufix(Xs,[Y|Ys]):-
	sufix(Xs,Ys).


% Para saber si una lista está dentro de otra comprobamos que
% que sea un prefijo de esta. Si no lo es quitamos el primer elemento
% la sublista Ys y vamos viendo entonces si es un prefijo de lo que queda.
sublist(Xs,Ys):-
	prefix(Xs,Ys).
sublist(Xs,[Y|Ys]):-
	sublist(Xs,Ys).

sublist_1(Xs,Ys):-
	prefix(Ps,ys),
	sufix(Xs,Ps).

sublist_2(Xs,Ys):-
	sufix(Ss,Ys),
	prefix(Xs,Ss).

sublist_3(Xs,PsXsSs):-
	append(Ps,XsSs,PsXsSs),
	append(Xs,Ss,XsSs).

sublist_4(Xs,PsXsSs):-
	append(PsXs,Ss,PsXsSs),
	append(Ps,Xs,PsXs).

nat(0).
nat(s(N)):-
	nat(N).

mylength([],0).
mylength([X|Xs],s(N)):-
	mylength(Xs,N).

%Toma dos listas y retira una ocurrencia de un elemento dado
myselect([X|Xs],X,Xs).
myselect([Y|Xs],X,[Y|Zs]):-
	myselect(Xs,X,Zs).

%Quita todas las ocurrencias de X en la lista dada.
% Aquí suponemos que el elemento a quitar es el primero de la lista,
% devlviendo Ys, que no sabemos si tiene ora ocurencia más.
mydelete([X|Xs],X,Ys):-
	mydelete(Xs,X,Ys).
% Hacemos llamada recursiva si los elementos de la cabeza no coinciden.
% decimos que no coinciden con X\=Z, y lo que hacemos es una llamada
% sin la cabeza de la lista: mydelete(Xs,Z,Ys).
mydelete([X|Xs],Z,[X|Ys]):-
	X\=Z,
	mydelete(Xs,Z,Ys).
% Este es el caso base
mydelete([],X,[]).

mypermutation([],[]).
mypermutation(Xs,[X|Zs]):-
	myselect(Xs,X,Ys),
	mypermutation(Ys,Zs).

double([],[]).
double([X|Xs],[X,X|s]):-
	double(Xs,Zs).
 %Un elemento no es miembro de una lista vacía
nomember(X,[]):-
	X\=[].

nomember(X,[Y|Ys]):-
	X\=Y,
	nomember(X,Ys).


% Devolvemos solo una vez cada elemento
nodoubles([],[]).
nodoubles([X|Xs],Zs):-
	member(X,Xs),
	nodoubles(Xs,Zs).
nodoubles([X|Xs],[X|Zs]):-
	nomember(X,Xs),
	nodoubles(Xs,Zs).
%Asociamos pares de listas. Estamos generando estructuras de datos con oficio(X,Y)|Zs)
associate([],[],[]).
associate([X|Xs],[Y|Ys],[oficio(X,Y)|Zs]):-
	associate(Xs,Ys,Zs).

mymember(X,[X|Xs]).
mymember(X,[Y|Ys]):-
	mymember(X,Ys).
% Da la vuelta a una lista
% es menos eficiente porque recorremos la lista 2 veces.
myreverse([],[]).
myreverse([X|Xs],Zs):-
	myreverse(Xs,Ws),
	append(Ws,[X],Zs).

% Pero esta forma es más eficiente, con uso de acumuladores

myreverse_ac(Xs,Ys):-
	myreverse_ac(Xs,[],Ys).

myreverse_ac([],Xs,Xs).
myreverse_ac([X|Xs],Acc,Ys):-
	myreverse_ac(Xs,[X|Acc],Ys).
	
