:-module(_,_).

% padece/2: predicado que es cierto si la persona indicada por el primer argumento padece la enfermedad
% indicada por el segundo argumento

padece(manuel,gripe).
padece(manuel,hepatitis).
padece(ignacio,hepatitis).
padece(ana,gripe).
padece(jose,intoxicacion).

% sintoma/2: predicado que es cierto si el sintoma indicado por el primer argumento es causado por la enfermedad
% indicada por el segundo argumento

sintoma(fiebre,gripe).
sintoma(cansancio,hepatitis).
sintoma(cansancio,gripe).
sintoma(diarrea,intoxicacion).

% suprime/2: predicado que es cierto si el medicamento indicado por el primer argumento suprime
% el sintoma indicado por el segundo argumento

suprime(aspirina,fiebre).
suprime(lomotil,diarrea).

% alivia/2: predicado que es cierto si el medicamento indicado por el primer argumento alivia la enfermedad
% representada por el segundo argumento. Esto sera asi si dicha enfermedad tiene un sintoma que sea suprimido 
% por el medicamento

alivia(M,E) :-
	   sintoma(S,E),
	   suprime(M,S).
	   
% debe_tomar/2: predicado que es cierto si la persona indicada por el primer argumento debe debe_tomar
% el medicamento indicado por el segundo argumento. Esto sera asi si dicha persona padece una enfermedad
% que sea aliviada por el medicamento

debe_tomar(P,M) :-
	  padece(F,E),
	  alivia(M,E).
	  
% preguntas
%
% ¿podemos conocer qué dolencia tiene Manuel?
% ?- padece(manuel,E).
%
% ¿Y Ana?
% ?- padece(ana,E).
%
% ¿Quién padece gripe?
% ?- padece(P,gripe).
%
% ¿Quién padece diarrea?
% ?- padece(P,diarrea).
%
% ¿Quién está cansado?
% ?- sintoma(cansancio,E), padece(P,E).
%
% ¿Hay algún medicamento que alivie a Manuel?
% ?- padece(manuel,E), alivia(M,E).
%
% ¿Hay algún síntoma que compartan Ignacio y Ana?
% ?- padece(ignacio,E1), padece(ana,E2), sintoma(S,E1), sintoma(S,E2).