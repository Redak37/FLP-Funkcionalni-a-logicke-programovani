/**
* Autor: Radek Duchoň, xducho07
* Řešení: Logický projekt FLP - Rubikova kostka
*/

/** Převzatá funkce */
/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** Převzatá funkce */
/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


/** Převzatá funkce */
read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).


/** Převzatá funkce */
/** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


/** Převzatá funkce */
/** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).



% trojice identifikační čísol stavu, předchozí stav ze kterého byl stav vygenerován, kostka
:-dynamic moves/3.


% vrátí obsah listů s řádky
radky([RA, RB, RC, RD], RA, RB, RC, RD, RA, RB, RC, RD).
rada([R], R, R).


% vráti orotovaonou stranu kostky doleva/doprava
rotateL([A1, A2, A3], [A4, A5, A6], [A7, A8, A9], [A3, A6, A9], [A2, A5, A8], [A1, A4, A7]).
rotateR([A1, A2, A3], [A4, A5, A6], [A7, A8, A9], [A7, A4, A1], [A8, A5, A2], [A9, A6, A3]).


% provádí rotace stranami dokud se nepovede kostku složit, v jednom zanoření vytvoří 12 kostek
rotate(Prev) :-
    moves(_, Prev, [L1, L2, L3, L4, L5, L6, L7, L8, L9]), % Načte další stav ze kterého generuje pohyby
    rada(L1, RA1, [A1, A2, A3]),
    rada(L2, RA2, [A4, A5, A6]),
    rada(L3, RA3, [A7, A8, A9]),
    radky(L4, RB1, RC1, RD1, RE1, [B1, B2, B3], [C1, C2, C3], [D1, D2, D3], [E1, E2, E3]),
    radky(L5, RB2, RC2, RD2, RE2, [B4, B5, B6], [C4, C5, C6], [D4, D5, D6], [E4, E5, E6]),
    radky(L6, RB3, RC3, RD3, RE3, [B7, B8, B9], [C7, C8, C9], [D7, D8, D9], [E7, E8, E9]),
    rada(L7, RF1, [F1, F2, F3]),
    rada(L8, RF2, [F4, F5, F6]),
    rada(L9, RF3, [F7, F8, F9]),
    
    rotateR(RE1, RE2, RE3, RER1, RER2, RER3),
    K1 = [[[D9, A2, A3]], [[D6, A5, A6]], [[D3, A8, A9]],
          [[A1, B2, B3], RC1, [D1, D2, F7], RER1], [[A4, B5, B6], RC2, [D4, D5, F4], RER2], [[A7, B8, B9], RC3, [D7, D8, F1], RER3],
          [[B1, F2, F3]], [[B4, F5, F6]], [[B7, F8, F9]]],
    rotateL(RE1, RE2, RE3, REL1, REL2, REL3),
    K2 = [[[B1, A2, A3]], [[B4, A5, A6]], [[B7, A8, A9]],
          [[F1, B2, B3], RC1, [D1, D2, A7], REL1], [[F4, B5, B6], RC2, [D4, D5, A4], REL2], [[F7, B8, B9], RC3, [D7, D8, A1], REL3],
          [[D9, F2, F3]], [[D6, F5, F6]], [[D3, F8, F9]]],
    
    rotateR(RC1, RC2, RC3, RCR1, RCR2, RCR3),
    K3 = [[[A1, A2, B3]], [[A4, A5, B6]], [[A7, A8, B9]],
          [[B1, B2, F3], RCR1, [A9, D2, D3], RE1], [[B4, B5, F6], RCR2, [A6, D5, D6], RE2], [[B7, B8, F9], RCR3, [A3, D8, D9], RE3],
          [[F1, F2, D7]], [[F4, F5, D4]], [[F7, F8, D1]]], 
    rotateL(RC1, RC2, RC3, RCL1, RCL2, RCL3),
    K4 = [[[A1, A2, D7]], [[A4, A5, D4]], [[A7, A8, D1]],
          [[B1, B2, A3], RCL1, [F9, D2, D3], RE1], [[B4, B5, A6], RCL2, [F6, D5, D6], RE2], [[B7, B8, A9], RCL3, [F3, D8, D9], RE3],
          [[F1, F2, B3]], [[F4, F5, B6]], [[F7, F8, B9]]],
    
    rotateR(RB1, RB2, RB3, RBR1, RBR2, RBR3),
    K5 = [[RA1], [RA2], [[E9, E6, E3]],
          [RBR1, [A7, C2, C3], RD1, [E1, E2, F1]], [RBR2, [A8, C5, C6], RD2, [E4, E5, F2]], [RBR3, [A9, C8, C9], RD3, [E7, E8, F3]],
          [[C7, C4, C1]], [[F4, F5, F6]], [[F7, F8, F9]]],
    rotateL(RB1, RB2, RB3, RBL1, RBL2, RBL3),
    K6 = [[RA1], [RA2], [[C1, C4, C7]],
          [RBL1, [F3, C2, C3], RD1, [E1, E2, A9]], [RBL2, [F2, C5, C6], RD2, [E4, E5, A8]], [RBL3, [F1, C8, C9], RD3, [E7, E8, A7]],
          [[E3, E6, E9]], [[F4, F5, F6]], [[F7, F8, F9]]],
    
    rotateR(RD1, RD2, RD3, RDR1, RDR2, RDR3),
    K7 = [[[C3, C6, C9]], [RA2], [RA3],
          [RB1, [C1, C2, F9], RDR1, [A3, E2, E3]], [RB2, [C4, C5, F8], RDR2, [A2, E5, E6]], [RB3, [C7, C8, F7], RDR3, [A1, E8, E9]],
          [[F1, F2, F3]], [[F4, F5, F6]], [[E1, E4, E7]]],
    rotateL(RD1, RD2, RD3, RDL1, RDL2, RDL3),
    K8 = [[[E7, E4, E1]], [RA2], [RA3],
          [RB1, [C1, C2, A1], RDL1, [F7, E2, E3]], [RB2, [C4, C5, A2], RDL2, [F8, E5, E6]], [RB3, [C7, C8, A3], RDL3, [F9, E8, E9]],
          [[F1, F2, F3]], [[F4, F5, F6]], [[C9, C6, C3]]],
    
    rotateR(RA1, RA2, RA3, RAR1, RAR2, RAR3),
    K9 = [[RAR1], [RAR2], [RAR3], [RC1, RD1, RE1, RB1], L5, L6, L7, L8, L9],
    rotateL(RA1, RA2, RA3, RAL1, RAL2, RAL3),
    K10 = [[RAL1], [RAL2], [RAL3], [RE1, RB1, RC1, RD1], L5, L6, L7, L8, L9],
    rotateR(RF1, RF2, RF3, RFR1, RFR2, RFR3),
    K11 = [L1, L2, L3, L4, L5, [RC3, RD3, RE3, RB3], [RFR1], [RFR2], [RFR3]],
    rotateL(RF1, RF2, RF3, RFL1, RFL2, RFL3),
    K12 = [L1, L2, L3, L4, L5, [RE3, RB3, RC3, RD3], [RFL1], [RFL2], [RFL3]],
   
    check_cubes(Prev, [K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11, K12]).



% Kontroluje aktuálně vygenerované kostky
check_cubes(Prev, []) :- Next is Prev + 1, rotate(Next). % Spustí další iteraci rotací
check_cubes(Prev, [H|T]) :- moves(_,_,H), check_cubes(Prev, T). % Ignoruje dříve viděné kostky
check_cubes(Prev, [H|_]) :- check_cube(H), moves(_,M,_), N is M + 1, asserta(moves(Prev, N, H)), write_cubes(Prev, H). % Pokud je kostka složená, vypíše cestu
check_cubes(Prev, [H|T]) :- moves(_,M,_), N is M + 1, asserta(moves(Prev, N, H)), check_cubes(Prev, T). % Vloží kostku do seznamu již viděných kostek
   

% Vypíše kroky od zadané kostky ke složené
write_cubes(0, C) :- write_cube(C).
write_cubes(P, C) :- moves(O, P, OC), write_cubes(O, OC), write('\n'), write_cube(C).


% Vypíše další část řádku kostky
write_next(T) :- write(' '), write_line(T).

% Vypíše řádek kostky
write_line([]) :- write('\n').
write_line([H|T]) :- atomic_list_concat(H, A), write(A), write_next(T).

% Vypíše kostku z vnitřní reprezentace
write_cube([]).
write_cube([H|T]) :- write_line(H), write_cube(T).

start :-
    prompt(_, ''),
    read_lines(LL),
    split_lines(LL,S),
    write_cube(S),
    % Zamezení hledání řešení vyřešené kostky
    (check_cube(S) -> (true);(
        write('\n'),
        assert(moves(0, 0, S)), !,
        rotate(0))),
    halt.


% vrací 4 stěny z řády kostky
rady([RA, RB, RC, RD], RA, RB, RC, RD).


% Kontroluje že je kostka složená
check_cube([L1, L2, _, L4, L5, L6|_]) :-
    rady(L4, [B, B, B], [C, C, C], [D, D ,D], [E, E, E]),
    rady(L5, [B, B, B], [C, C, C], [D, D, D], [E, E, E]),
    rady(L6, [B, B, B], [C, C, C], [D, D, D], [E, E, E]),
    L1 = L2.
