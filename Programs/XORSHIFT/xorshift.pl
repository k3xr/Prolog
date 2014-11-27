:-module(_,_).

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
% Define a byte type either as a binary byte or as an hex byte.
byte(BB) :-
	binary_byte(BB).


byte(HB) :-
	hex_byte(HB).
	
% Define an hex byte as a list of 2 hex nibbles.
hex_byte([hexd(H1), hexd(H0)]) :-
	hexd(H1),	
	hexd(H0).
	
% IMPLEMENTAR:
% xorshift_encrypt()
% xorshift_decrypt()

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
	

% get_nth_bit_from_byte(N, B, BN)
% byte_list_clsh(L, CLShL)
% byte_list_crsh(L, CRShL)
% byte_xor(B1, B2, B3)
% xorshift_encrypt(ClearData, EncKey, EncData)
% xorshift_decrypt(EncData, EncKey, ClearData)

% byte_list(L)
% Este predicado es cierto si la lista dada en el primer argumento es una lista de bytes (ya sea binarios o hex). 
% SE ASUME QUE EL PRIMER ELEMENTO DE LA LISTA ES EL BIT MÁS SIGNIFICATIVO, MIENTRAS QUE EL ÚLTIMO ELEMENTO DE LA LISTA SERÍA EL BIT MENOS SIGNIFICATIVO.

byte_list([]).
byte_list([L|Ls]) :-
	byte(L),
	byte_list(Ls).

% byte_list_conversion(HL, BL)
% Este predicado es cierto si la representación binaria de la lista de bytes hexadecimales 
% que aparece en el primer argumento es la lista de bytes que aparece en el segundo argumento.

byte_list_conversion([],[]).
byte_list_conversion([HL|HLs], [BL|BLs]) :-
	byte_list([HL|HLs]),
	byte_list([BL|BLs]),
	byte_conversion(HL,BL),
	byte_list_conversion(HLs,BLs).