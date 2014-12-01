module(_,_).

% Define a binary digit type.
bind(0).
bind(1).

% Define a binary byte as a list of 8 binary digits.
binary_byte([bind(B7), bind(B6), bind(B5), bind(B4), bind(B3), bind(B2), bind(B1), bind(B0)]) :-
	bind(B7),
	bind(B6),
	bind(B5),
	bind(B4),
	bind(B3),
	bind(B2),
	bind(B1),
	bind(B0).
	
% Define an hex digit (nibble) type.
hexd(0).
hexd(1).
hexd(2).
hexd(3).
hexd(4).
hexd(5).
hexd(6).
hexd(7).
hexd(8).
hexd(9).
hexd(a).
hexd(b).
hexd(c).
hexd(d).
hexd(e).
hexd(f).

hex_0(hexd(0)).
hex_1(hexd(1)).
hex_2(hexd(2)).
hex_3(hexd(3)).
hex_4(hexd(4)).
hex_5(hexd(5)).
hex_6(hexd(6)).
hex_7(hexd(7)).
hex_8(hexd(8)).
hex_9(hexd(9)).
hex_a(hexd(a)).
hex_b(hexd(b)).
hex_c(hexd(c)).
hex_d(hexd(d)).
hex_e(hexd(e)).
hex_f(hexd(f)).

bin_0([bind(0),bind(0),bind(0),bind(0)]).
bin_1([bind(0),bind(0),bind(0),bind(1)]).
bin_2([bind(0),bind(0),bind(1),bind(0)]).
bin_3([bind(0),bind(0),bind(1),bind(1)]).
bin_4([bind(0),bind(1),bind(0),bind(0)]).
bin_5([bind(0),bind(1),bind(0),bind(1)]).
bin_6([bind(0),bind(1),bind(1),bind(0)]).
bin_7([bind(0),bind(1),bind(1),bind(1)]).
bin_8([bind(1),bind(0),bind(0),bind(0)]).
bin_9([bind(1),bind(0),bind(0),bind(1)]).
bin_a([bind(1),bind(0),bind(1),bind(0)]).
bin_b([bind(1),bind(0),bind(1),bind(1)]).
bin_c([bind(1),bind(1),bind(0),bind(0)]).
bin_d([bind(1),bind(1),bind(0),bind(1)]).
bin_e([bind(1),bind(1),bind(1),bind(0)]).
bin_f([bind(1),bind(1),bind(1),bind(1)]).

% XOR Gate
xor([bind(0), bind(0), bind(0)]).
xor([bind(0), bind(1), bind(1)]).
xor([bind(1), bind(0), bind(1)]).
xor([bind(1), bind(1), bind(0)]).

% Define a byte type either as a binary byte or as an hex byte.
byte(BB) :-
	binary_byte(BB).
byte(HB) :-
	hex_byte(HB).
	
% Define an hex byte as a list of 2 hex nibbles.
hex_byte([hexd(H1), hexd(H0)]) :-
	hexd(H1),	
	hexd(H0).
	
% TODO:
% xorshift_encrypt(ClearData, EncKey, EncData)
% xorshift_decrypt(EncData, EncKey, ClearData)

% byte_list(L)
% Este predicado es cierto si la lista dada en el primer argumento es una lista de bytes (ya sea binarios o hex). 
% SE ASUME QUE EL PRIMER ELEMENTO DE LA LISTA ES EL BIT MÁS SIGNIFICATIVO, MIENTRAS QUE EL ÚLTIMO ELEMENTO DE LA LISTA SERÍA EL BIT MENOS SIGNIFICATIVO.

byte_list([]).
byte_list([L|Ls]) :-
	byte(L),
	byte_list(Ls).
	
% byte_conversion(HexByte, BinByte)
% Este predicado es cierto si el byte hexadecimal que aparece en el primer argumento 
% tiene como representación binaria el byte binario que aparece en el segundo argumento.
byte_conversion([H1,H0],[B7,B6,B5,B4|BL]):-
	byte([H1,H0]),
	byte([B7,B6,B5,B4|BL]),
	nibble_conversion(H1,[B7,B6,B5,B4]),
	nibble_conversion(H0,BL).

nibble_conversion(H,B):-
	(hex_0(H),bin_0(B));
	(hex_1(H),bin_1(B));
	(hex_2(H),bin_2(B));
	(hex_3(H),bin_3(B));
	(hex_4(H),bin_4(B));
	(hex_5(H),bin_5(B));
	(hex_6(H),bin_6(B));
	(hex_7(H),bin_7(B));
	(hex_8(H),bin_8(B));
	(hex_9(H),bin_9(B));
	(hex_a(H),bin_a(B));
	(hex_b(H),bin_b(B));
	(hex_c(H),bin_c(B));
	(hex_d(H),bin_d(B));
	(hex_e(H),bin_e(B));
	(hex_f(H),bin_f(B)).

% byte_list_conversion(HL, BL)
% Este predicado es cierto si la representación binaria de la lista de bytes hexadecimales 
% que aparece en el primer argumento es la lista de bytes que aparece en el segundo argumento.

byte_list_conversion([],[]).
byte_list_conversion([HL|HLs], [BL|BLs]) :-
	byte_conversion(HL,BL),
	byte_list_conversion(HLs,BLs).
	
% get_nth_bit_from_byte(N, B, BN)
% Este predicado POLIMÓRFICO es cierto si BN es el dígito binario (bit) número N (N ES UN NÚMERO DE PEANO) del byte B 
% ya sea este un byte hexadecimal o binario). NOTA: EL ÍNDICE DEL BIT MENOS SIGNIFICATIVO DE UN BYTE NO ES 1, SINO 0.

get_nth_bit_from_byte(0, [B|_], B).	
get_nth_bit_from_byte(s(N), [_|Bs], BN) :-
	get_nth_bit_from_byte(N,Bs,BN).
	
% byte_list_clsh(L, CLShL)
% Este predicado POLIMÓRFICO es cierto si CLShL es el resultado de efectuar un desplazamiento circular hacia 
% la izquierda de la lista de bytes representada por L. Este predicado debe funcionar tanto para listas de bytes
% hexadecimales como binarias, aunque ambos argumentos deben estar representados EN LA MISMA NOTACIÓN. 
% En los desplazamientos circulares a la izquierda el bit más significativo del byte más significativo de la lista 
% L pasa a ser el bit menos significativo del byte menos significativo de la lista CLShL.
	
byte_list_clsh([Byte|L], ShiftedAndFormated) :-
	%Si es binario.
	binary_byte(Byte),
	my_append([Byte|L],Lappended),
	shift(Lappended,Shifted),
	group_bit_in_B(Shifted, ShiftedAndFormated).

byte_list_clsh([Byte|L], ShiftedAndFormatedFromHex) :-
	%Si es hexadecimal -> Pasamos a binario
	hex_byte(Byte),
	byte_list_conversion([Byte|L], BL),
	my_append(BL,Lappended),
	shift(Lappended,Shifted),
	group_bit_in_B(Shifted, ShiftedAndFormated),
	byte_list_conversion(ShiftedAndFormatedFromHex, ShiftedAndFormated).

%*********************Auxiliar Methods for rotation purposes*************************	
% Group 8 bytes in a list
group_bit_in_B([],[]).
group_bit_in_B([B7,B6,B5,B4,B3,B2,B1,B0|Lbits],[[B7,B6,B5,B4,B3,B2,B1,B0]|LBytes]):-
	group_bit_in_B(Lbits, LBytes).	
	
my_append([], []).
my_append([L|Ls], As) :-
    append_tres(L, Ws, As),
    my_append(Ls, Ws).
 
% Append two list in a third one.
append_tres([], L, L).
append_tres([H|T], L, [H|R]) :-
    append_tres(T, L, R).
	
% shift(List1?,List2?)
% L2 is the left-shifted of L1 OR L1 is the rigth-shifted of L2. L1 and L2 lists.
shift(L1,L2):-
    del(L1,L3),
    add(L3,L1,L2).
     
del([_|Tail],Tail).
     
add([],[Head|_],[Head]).
add([Head|Tail],L1,[Head|L2]):-
    add(Tail,L1,L2).
%********************* FIN: Auxiliar Methods for rotation purposes*************************	

	
% byte_list_crsh/2: byte_list_crsh(L, CRShL).
% Este predicado POLIMÓRFICO es cierto si CRShL es el resultado de efectuar un 
% desplazamiento circular hacia la derecha de la lista de bytes representada por L. 
% Este predicado debe funcionar tanto para listas de bytes hexadecimales como binarios, 
% aunque ambos argumentos deben estar representados EN LA MISMA NOTACIÓN. En los 
% desplazamientos circulares a la derecha el bit menos significativo del byte menos 
% significativo de L pasa a ser el bit más significativo del byte mas significativo de 
% la lista CRShL.

byte_list_crsh([Byte|L], ShiftedAndFormated) :-
	%Si es binario.
	binary_byte(Byte),
	my_append([Byte|L],Lappended),
	shift(Shifted, Lappended),
	group_bit_in_B(Shifted, ShiftedAndFormated).
	
byte_list_crsh([Byte|L], ShiftedAndFormatedFromHex) :-
	%Si es hexadecimal -> Pasamos a binario
	hex_byte(Byte),
	byte_list_conversion([Byte|L], BL),
	my_append(BL,Lappended),
	shift(Shifted, Lappended),
	group_bit_in_B(Shifted, ShiftedAndFormated),
	byte_list_conversion(ShiftedAndFormatedFromHex, ShiftedAndFormated).
	
% byte_xor(B1, B2, B3)
% Este predicado POLIMÓRFICO es cierto si B3 es el resultado de efectuar la operación lógica XOR 
% entre los bytes B1 y B2. Este predicado debe funcionar tanto para bytes binarios como hexadecimales, 
% aunque todos los argumentos deben estar representados EN LA MISMA NOTACIÓN.

byte_xor([], [] ,[]).
byte_xor(B1, B2, B3):-
	hex_byte(B1),
	hex_byte(B2),
	byte_conversion(B1, BB1),
	byte_conversion(B2, BB2),
	xor_gate(BB1, BB2, BBOutput),
	byte_conversion(B3, BBOutput).

byte_xor(B1, B2, B3):-
	binary_byte(B1),
	binary_byte(B2),
	xor_gate(B1, B2, B3). 

xor_gate([], [],[]).
xor_gate([BitB1|B1], [BitB2|B2], [BitB3|B3]):-
	xor([BitB1, BitB2, BitB3]),
	xor_gate(B1, B2, B3).
	
	