% u110340 Fco. Javier Caballero Abenza
% t110118 Jorge Gonzalez Valencia
% s100077 Marco Lopez de Miguel

% Parte 1: HiperCubos -----------------------------------------------------------------------

	%Calcula el factorial de un número. 
	%Nos permitirá saber el número de elementos de dimension m que tiene un hipercubo de dimension m.
	%Caso base 0!=1
	factorial(0,1).
	factorial(N,F) :-
              N > 0,
			  M is N - 1,
			  factorial(M,G),
			  F is N*G.
			  
	%Calcula coeficientes binomiales, consideramos la notación típica matematica de M sobre N.
	combinatorio(M,N,Result):-
				M>=N,
				factorial(M, FactorialM),
				factorial(N, FactorialN),
				R is M - N,
				factorial(R, FactorialR),
				Divisor is FactorialN * FactorialR,
				Result is FactorialM / Divisor.
	
	%Utilizamos la funcion citada en la memoria para obtener los elementos de dimension M un hipercubo de dimension N
	hipercubo(N,Lista):-
				M = N,
				aLista(N,M,Lista).
				
	
	calculaElementos(N, M, Result):-
				X is N-M,
				F is 2^X,
				combinatorio(N,M,ROperacion),
				Result is F*ROperacion.
			
	%Caso base
	aLista(N,M,Lista):-
				M = 0,
				calculaElementos(N,M,Result),
				Lista = [Result].
				
	aLista(N,M,L):-
				M > 0,
				M_nuevo is M - 1,
				aLista(N,M_nuevo,F),
				calculaElementos(N,M,Result),
				Lelemento = [Result],
				append(Lelemento, F, Lista).
	