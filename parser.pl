regex_match(Regex, Hilera, Match) :- string_chars(Regex, RegexChars),
									 parse_regex(RegexChars).
									 %retractall(estado(X)),
									 %retractall(ultimo_estado(X)),
									 %retractall(transicion(X, Y, Z)).
retract_all() :- retractall(estado(X)),
				 retractall(ultimo_estado(X)),
				 retractall(transicion(X, Y, Z)).

parse_regex(Regex) :- assert(estado(0)), assert(estado(1)), assert(ultimo_estado(1)), parse(Regex, [[0,1]], [0, 0]).

parse([], [[_, CierraEstado] | _], [UltimoEstado, _]) :- crear_transicion('¬', UltimoEstado, CierraEstado).
parse([')','*'|Xr], [[AbreEstado, CierraEstado] | EstadosAfuera], [UltimoEstado, PenultimoEstado]):-
																	estrella(AbreEstado, CierraEstado, NuevoUltimoEstado), 
																 	parse(Xr, EstadosAfuera, [NuevoUltimoEstado, CierraEstado]).
parse([')'|Xr], [[_, CierraEstado]|EstadosAfuera], [UltimoEstado, PenultimoEstado]) :- crear_transicion('¬', UltimoEstado, CierraEstado),
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

% [[q0,nil,q1],[q1,X,q2],[q2,nil,q3],[q0,nil,q3],[q3,nil,q1]]
estrella(PrimerEstado, UltimoEstado, NuevoUltimoEstado) :- nuevo_estado(Intermedio),
														   nuevo_estado(NuevoUltimoEstado),
														   retract_trans(PrimerEstado, L),
														   insert_trans(PrimerEstado, L, Intermedio),
														   crear_transicion('¬', UltimoEstado, NuevoUltimoEstado),
														   crear_transicion('¬', PrimerEstado, NuevoUltimoEstado),
														   insert_trans(NuevoUltimoEstado, L, Intermedio).

devolverse(EstadoFinal, [[E|_]|Xr], EstadoInicial) :- crear_transicion('¬', EstadoFinal, E), devolverse(EstadoFinal, Xr).

% t(Primer, T, E)
insert_trans(_, [], _).
insert_trans(Primer, [[E, T] | Xr], Intermedio) :- crear_transicion('¬', Primer, Intermedio), crear_transicion(T, Intermedio, E), 
												   insert_trans(Primer, Xr, Intermedio).


retract_trans(X, []) :- \+transicion(X, T, E).
retract_trans(X, [[E, T]|L1]) :- transicion(X, T, E), retract(transicion(X, T, E)), retract_trans(X, L1).

nuevo_estado(Nuevo) :- ultimo_estado(E), retract(ultimo_estado(E)), Nuevo is E + 1, 
					   assert(ultimo_estado(Nuevo)), assert(estado(Nuevo)).
