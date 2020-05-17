regex_match(Regex, Hilera, Match) :- retract_all(),
									 string_chars(Regex, RegexChars),
									 parse_regex(RegexChars),
									 string_chars(Hilera, HileraChars),
									 estado_inicial(X),
									 accept_string(HileraChars, MatchChars, X),
									 !, estado_aceptado,
									 string_chars(Match, MatchChars).

accept_string(_, [], Actual) :- estado_aceptacion(Actual), assert(estado_aceptado), !.
accept_string([], Match, Actual) :- findall(Y, transicion(Actual, '¬', Y), L),
									explore_transitions([], '¬', Actual, L, Match).
accept_string([], [], _).
accept_string([X|Xr], Match, Actual) :- findall(Y, transicion(Actual, X, Y), L1),
										findall(Z, transicion(Actual, '¬', Z), L2),
										(explore_transitions([X|Xr], X, Actual, L1, Match);
										explore_transitions([X|Xr], '¬', Actual, L2, Match)).
accept_string([_|Xr], Match, _) :- estado_inicial(I), accept_string(Xr, Match, I).

explore_transitions(_, _, Actual, _, []) :- estado_aceptacion(Actual), !.
explore_transitions(Hilera, '¬', _, [Y|_], Match) :- accept_string(Hilera, Match, Y), estado_aceptado.
explore_transitions(Hilera, '¬', Actual, [_|Yr], Match) :- explore_transitions(Hilera, '¬', Actual, Yr, Match), !.
explore_transitions([X|Xr], _, _, [Y|_], [X|Match]) :- accept_string(Xr, Match, Y), estado_aceptado.
explore_transitions([X|Xr], _, Actual, [_|Yr], [X|Match]) :- explore_transitions([X|Xr], X, Actual, Yr, Match).

retract_all() :- retractall(estado(_)),
				 retractall(estado_aceptacion(_)),
				 retractall(ultimo_estado(_)),
				 retractall(transicion(_, _, _)),
				 retractall(estado_aceptado).

parse_regex(Regex) :- assert(estado(0)), assert(estado_inicial(0)), assert(estado(1)), assert(ultimo_estado(1)), 
					  assert(estado_aceptacion(1)), parse(Regex, [[0,1]], [0, 0]).

parse([], [[_, CierraEstado] | _], [UltimoEstado, _]) :- crear_transicion('¬', UltimoEstado, CierraEstado).
parse([')','*'|Xr], [[AbreEstado, CierraEstado] | EstadosAfuera], _):-
																	estrella(AbreEstado, CierraEstado, NuevoUltimoEstado), 
																 	parse(Xr, EstadosAfuera, [NuevoUltimoEstado, CierraEstado]).
parse([')'|Xr], [[_, CierraEstado]|EstadosAfuera], [UltimoEstado, _]) :- crear_transicion('¬', UltimoEstado, CierraEstado),
																	   parse(Xr, EstadosAfuera, [CierraEstado, UltimoEstado]).
parse(['('|Xr], EstadosAfuera, [UltimoEstado, PenultimoEstado]) :- nuevo_estado(N), parse(Xr, [[UltimoEstado, N] | EstadosAfuera], [UltimoEstado, PenultimoEstado]).
parse(['|'|Xr], [[AbreEstado, CierraEstado] | EstadosAfuera], [UltimoEstado, _]) :- crear_transicion('¬', UltimoEstado, CierraEstado),
																					parse(Xr, [[AbreEstado, CierraEstado] | EstadosAfuera], [AbreEstado, UltimoEstado]).
parse(['*'|Xr], EstadosAfuera, [UltimoEstado, PenultimoEstado]) :- estrella(PenultimoEstado, UltimoEstado, NuevoUltimoEstado), 
																   parse(Xr, EstadosAfuera, [NuevoUltimoEstado, UltimoEstado]).
parse([X|Xr], EstadosAntesParen, [UltimoEstado, _]) :- nuevo_estado(Trans), crear_transicion(X, UltimoEstado, Trans), 
													   parse(Xr, EstadosAntesParen, [Trans, UltimoEstado]).

crear_transicion(Caracter, EstadoInicial, EstadoFinal) :- transicion(EstadoInicial, Caracter, EstadoFinal).
crear_transicion(Caracter, EstadoInicial, EstadoFinal) :- assert(transicion(EstadoInicial, Caracter, EstadoFinal)).

% [[PrimerEstado,nil,Intermedio],[Intermedio,X,UltimoEstado],[UltimoEstado,nil,NuevoUltimoEstado],
% [PrimerEstado,nil,NuevoUltimoEstado],[NuevoUltimoEstado,nil,Intermedio]]
estrella(PrimerEstado, UltimoEstado, NuevoUltimoEstado) :- nuevo_estado(Intermedio),
														   nuevo_estado(NuevoUltimoEstado),
														   retract_trans(PrimerEstado, L),
														   insert_trans(PrimerEstado, L, Intermedio),
														   crear_transicion('¬', UltimoEstado, NuevoUltimoEstado),
														   crear_transicion('¬', PrimerEstado, NuevoUltimoEstado),
														   crear_transicion('¬', NuevoUltimoEstado, Intermedio).

devolverse(EstadoFinal, [[E|_]|Xr], _) :- crear_transicion('¬', EstadoFinal, E), devolverse(EstadoFinal, Xr).

% t(Primer, T, E)
insert_trans(_, [], _).
insert_trans(Primer, [[E, T] | Xr], Intermedio) :- crear_transicion('¬', Primer, Intermedio), crear_transicion(T, Intermedio, E), 
												   insert_trans(Primer, Xr, Intermedio).


retract_trans(X, []) :- \+transicion(X, _, _).
retract_trans(X, [[E, T]|L1]) :- transicion(X, T, E), retract(transicion(X, T, E)), retract_trans(X, L1).

nuevo_estado(Nuevo) :- ultimo_estado(E), retract(ultimo_estado(E)), Nuevo is E + 1, 
					   assert(ultimo_estado(Nuevo)), assert(estado(Nuevo)).
