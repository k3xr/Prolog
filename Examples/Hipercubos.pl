% u110340 Fco. Javier Caballero Abenza
% t110118 Jorge Gonzalez Valencia
% s100077 Marco Lopez de Miguel

% Parte 1: HiperCubos -----------------------------------------------------------------------
     
    %Predicado para calcular el factorial de un numero, que usaremos mas adelante para saber
    %cuantos elementos de dimension m hay en un hipercubo de dimension n
    fact(0,1):-!.
    fact(X,L):-
            X>0,
            Y is X-1,
            fact(Y,M),
            L is X * M.
     
    %Predicado para calcular el numero combinatorio (n sobre m)
    comb(N,M,X):-
            fact(N,A),
            fact(M,B),
            C is N - M,
            fact(C,D),
            E is D * B,
            X is A / E.
     
    %Predicado que se encarga de calcular el numero en si.
    %La formula que utiliza es:
    %Elementos de dim M en un hipercubo de orden N: 2^(n-m) * (n sobre m)
    valor(DIMN, ELEMDIMM, VAL):-
            X is DIMN - ELEMDIMM,
            Y is 2 ^ X,
            comb(DIMN, ELEMDIMM, A),
            VAL is Y * A.
     
    %Hipercubo llama a un metodo auxiliar que se encarga de crear la lista
    hipercubo(N,L):-
            X = N,
            actualizar(N,X,L).
     
    %Caso base de actualizar. Creamos una lista con un unico elemento cuyo valor es el numero de elementos de dim 0
    actualizar(N,X,L):-
            X = 0,
            valor(N,X,Y),
            L = [Y].
           
    %En la llamada recursiva, creamos una nueva lista cuyo primer elemento es
    %el numero de elementos de dimension X, y lo enlazamos a la lista actualizar de X-1
    actualizar(N,X,L):-
            X > 0,
            X1 is X - 1,
            actualizar(N,X1,F),
            valor(N,X,J),
            JL = [J],
            append(JL, F, L).
           
    %Para sacar los elementos de un hipercubo de dimension 12, metemos el comando
    %?- hipercubo(12,L).
    %L = [1, 24, 264, 1760, 7920, 25344, 59136, 101376, 126720|...] ;
    %false.
    %Al ser una lista con numeros grandes, no nos imprime todos por pantalla, pero podemos
    %sacar los siguientes manualmente con los siguientes comandos
    %?- valor(12,3,X).
    %X = 112640.
    %?- valor(12,2,X).
    %X = 67584.
    %?- valor(12,1,X).
    %X = 24576.
    %?- valor(12,0,X).
    %X = 4096.
     

% Parte 2: Detectando ciclos en grafos -----------------------------------------------------------------------

    %Ejemplo 1
    grafo(seta, [l(a,b), l(b,c), l(c,d), l(c,b), l(d,a)]).
	%Ejemplo 2
    grafo(t, [l(a,b), l(c,d), l(c,b), l(d,a)]).
     
     
    last([X], X).
    last([_|T], L):-
            last(T, L).
     
    hay_ciclo(Nombre, Recorrido):-
            grafo(Nombre, Aristas),
            member(X,Aristas),
            arg(1,X,X1),
            arg(2,X,X2),
            PrimerCaso = [X1, X2],
            %Creamos una primera lista que contiene cada vertice recorrido,
            %Empezando con los vertices pertenecientes a cada arista
            auxiliar(PrimerCaso, Aristas, Recorrido).
           
    auxiliar(Evaluado, Aristas, Recorrido):-
            last(Evaluado, Ultimo),
            member(A, Aristas),
            A = l(Ultimo, Siguiente),
            member(Siguiente, Evaluado),
            %Si el posible miembro al que podemos ir ya lo hemos visitado, hay un ciclo, asi que lo devolvemos
            append(Evaluado, [Siguiente], Recorrido).
           
    auxiliar(Evaluado, Aristas, Recorrido):-
            last(Evaluado, Ultimo),
            member(A, Aristas),
            A = l(Ultimo, Siguiente),
            \+ member(Siguiente, Evaluado),
            %Si el posible miembro al que podemos ir no lo hemos visitado, añadimos y miramos todas las nuevas posibilidadas
            append(Evaluado, [Siguiente], Continuacion),
            auxiliar(Continuacion, Aristas, Recorrido).
           
           
% Parte 3: Decodificando los genes -----------------------------------------------------------------------
     
    %Hechos que contienen todas las posibles formaciones de aminoacidos
     
    am("gcu","ala").
    am("gcc","ala").
    am("gca","ala").
    am("gcg","ala").
    am("aga","arg").
    am("agg","arg").
    am("cgu","arg").
    am("cgc","arg").
    am("cga","arg").
    am("cgg","arg").
    am("aau","asn").
    am("aac","asn").
    am("gau","asp").
    am("gac","asp").
    am("ugu","cys").
    am("ugc","cys").
    am("caa","gln").
    am("cag","gln").
    am("gaa","glu").
    am("gag","glu").
    am("ggu","gly").
    am("ggc","gly").
    am("gga","gly").
    am("ggg","gly").
    am("cau","his").
    am("cac","his").
    am("auu","ile").
    am("auc","ile").
    am("aua","ile").
    am("cuu","leu").
    am("cuc","leu").
    am("cua","leu").
    am("cug","leu").
    am("uua","leu").
    am("uug","leu").
    am("aaa","lys").
    am("aag","lys").
    am("aug","met").
    am("uuu","phe").
    am("uuc","phe").
    am("ccu","pro").
    am("ccc","pro").
    am("cca","pro").
    am("ccg","pro").
    am("agu","ser").
    am("agc","ser").
    am("ucu","ser").
    am("ucc","ser").
    am("uca","ser").
    am("ucg","ser").
    am("uaa","stop").
    am("uag","stop").
    am("uga","stop").
    am("acu","thr").
    am("acc","thr").
    am("aca","thr").
    am("acg","thr").
    am("ugg","trp").
    am("uau","tyr").
    am("uac","tyr").
    am("guu","val").
    am("guc","val").
    am("gua","val").
    am("gug","val").
     
    %Calcular la longitud de una lista
    len([],X):- X is 0.
    len([_|B],X):-
            len(B,Y),
            X is 1 + Y.
           
    %devuelve el resto de una lista al quitar el primer elemento
    saltar1([_|B],B):-
            len(B,LENB),
            LENB > 2.
           
    %devuelve el resto de una lista al quitar los dos primeros elementos
    saltar2([_|B],X):-
            B = [_|X],
            len(X,LENX),
            LENX > 2.
           
    %Devuelve los posibles aminoacidos que forma una cadena de ARN.
    aminoacidos(ARN,Cadenas):-
            comienza(ARN,SINELPRINCIPIO),
            len(SINELPRINCIPIO, LONG),
            componer(SINELPRINCIPIO, LONG, CadenasLista),
            %string_to_list(Cadenas,CadenasLista).
            meterGuiones(CadenasLista, CadenaConGuiones),
            %meterGuiones(CadenasLista, CadenaConGuiones),
            string_to_list(Cadenas, CadenaConGuiones).
     
    %Dada una cadena de ARN, devuelve la secuencia restante tras quitar el aminoacido que la inicializa, aug
    comienza(ARN,C):-
            %La cadena comienza en algun momento si tiene "aug" por algun lugar.
            %aug en ASCII es 97 117 103
            ARN = [97|A],
            A = [117|B],
            B = [103|C].
    comienza(ARN,C):-
            saltar1(ARN,ARNSALT1),
            comienza(ARNSALT1,C).
     
    %metodo que comprueba si un string de 3 caracteres termina como un aminoacido de STOP
    comprobartermina(S):- %si termina en uag
            len(S,LS),
            LS = 3,
            S = [117|A],
            A = [97|B],
            B = [103],!.
    comprobartermina(S):- %si termina en uaa
            len(S,LS),
            LS = 3,
            S = [117|A],
            A = [97|B],
            B = [97],!.
    comprobartermina(S):- %si termina en iga
            len(S,LS),
            LS = 3,
            S = [117|A],
            A = [103|B],
            B = [97],!.
           
    componer(Lista, Longitud, Cadenas):-
            Longitud = 3,
            comprobartermina(Lista),
            Cadenas = [].
    componer(Lista, Longitud, Cadenas):-
            Longitud = 4,
            saltar1(Lista, LAUX),
            comprobartermina(LAUX),
            Cadenas = [].
    componer(Lista, Longitud, Cadenas):-
            Longitud = 5,
            saltar2(Lista, LAUX),
            comprobartermina(LAUX),
            Cadenas = [].
    componer(Lista, Longitud, Cadenas):-
            Longitud > 5,
            Lista = [A1|X],
            X = [A2|Y],
            Y = [A3|RESTOCADENA],
            am([A1,A2,A3],AMINOACIDO),
            NextLong is Longitud - 3,
            componer(RESTOCADENA, NextLong, CadenaRestante),
            append(AMINOACIDO,CadenaRestante,Cadenas).
    componer(Lista, Longitud, Cadenas):-
            Longitud > 5,
            saltar1(Lista,ListaAUX),
            ListaAUX = [A1|X],
            X = [A2|Y],
            Y = [A3|RESTOCADENA],
            am([A1,A2,A3],AMINOACIDO),
            NextLong is Longitud - 4,
            componer(RESTOCADENA, NextLong, CadenaRestante),
            append(AMINOACIDO,CadenaRestante,Cadenas).
    componer(Lista, Longitud, Cadenas):-
            Longitud > 5,
            saltar2(Lista,ListaAUX),
            ListaAUX = [A1|X],
            X = [A2|Y],
            Y = [A3|RESTOCADENA],
            am([A1,A2,A3],AMINOACIDO),
            NextLong is Longitud - 5,
            componer(RESTOCADENA, NextLong, CadenaRestante),
            append(AMINOACIDO,CadenaRestante,Cadenas).
           
    %Metemos un guion cada tres letras para separar los aminoacidos (la cadena siempre tendra una longitud multiplo de 3)
    meterGuiones([A1|B], X):-
            B = [_|C],
            C = [_|D],
            D == [],
            X = [A1|B],!.
    meterGuiones([A1|B], X):-
            B = [A2|C],
            C = [A3|D],
            D \= [],
            ListaAux = [A1,A2,A3,45], %45 es el guion en ASCII
            meterGuiones(D,F),
            append(ListaAux, F, X).

