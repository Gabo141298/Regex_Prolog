regex_match(Regex, Hilera, Match) :- string_chars(Regex, RegexChars),
									 parse_regex(RegexChars).
									 %retractall(estado(X)),
									 %retractall(ultimo_estado(X)),
									 %retractall(transicion(X, Y, Z)).
retract_all() :- retractall(estado(X)),
				 retractall(ultimo_estado(X)),
				 retractall(transicion(X, Y, Z)).

parse_regex(Regex) :- assert(ultimo_estado(0)), parse(Regex, [0], [0, 0]).

parse([], _, _).
parse([')','*'|Xr], [PrimerEstado | EstadosAfuera], [UltimoEstado, PenultimoEstado]):- estrella(PrimerEstado, UltimoEstado, NuevoUltimoEstado), 
																 	parse(Xr, EstadosAfuera, [NuevoUltimoEstado, UltimoEstado, PenultimoEstado]).
parse([')'|Xr], [_|EstadosAfuera], UltimosEstados) :- parse(Xr, EstadosAfuera, UltimosEstados).
parse(['('|Xr], EstadosAfuera, [UltimoEstado, PenultimoEstado]) :- parse(Xr, [UltimoEstado | EstadosAfuera], [UltimoEstado, PenultimoEstado]).
parse(['|'|Xr], [PrimerEstado|EstadosAfuera], [UltimoEstado, _]) :- parse(Xr, [PrimerEstado|EstadosAfuera], [PrimerEstado, UltimoEstado]).
parse(['*'|Xr], _, [UltimoEstado, PenultimoEstado]) :- estrella(PenultimoEstado, UltimoEstado, NuevoUltimoEstado), 
									parse(Xr, EstadosAfuera, [NuevoUltimoEstado, UltimoEstado]).
parse([X|Xr], EstadosAntesParen, [UltimoEstado, _]) :- nuevo_estado(Trans), crear_transicion(X, UltimoEstado, Trans), parse(Xr, EstadosAntesParen, [Trans, UltimoEstado]).

crear_transicion(Caracter, EstadoInicial, EstadoFinal) :- assert(transicion(EstadoInicial, Caracter, EstadoFinal)), 
														  write(assert(transicion(EstadoInicial, Caracter, EstadoFinal))).

% [[q0,nil,q1],[q1,X,q2],[q2,nil,q3],[q0,nil,q3],[q3,nil,q1]]
estrella(PrimerEstado, UltimoEstado, NuevoUltimoEstado) :- write("estrella\n"),
														   nuevo_estado(Intermedio),
														   nuevo_estado(NuevoUltimoEstado),
														   retract_trans(PrimerEstado, L),
														   write("retract-insert " + a(PrimerEstado, L, Intermedio) + "\n"),
														   insert_trans(PrimerEstado, L, Intermedio),
														   crear_transicion('¬', UltimoEstado, NuevoUltimoEstado),
														   crear_transicion('¬', PrimerEstado, NuevoUltimoEstado),
														   insert_trans(NuevoUltimoEstado, L, Intermedio).

devolverse(EstadoFinal, [[E|_]|Xr], EstadoInicial) :- crear_transicion('¬', EstadoFinal, E), devolverse(EstadoFinal, Xr).

% t(Primer, T, E)
insert_trans(_, [], _). % :- write("termina insert\n").
insert_trans(Primer, [[E, T] | Xr], Intermedio) :- write("insert:" + a(Primer, ¬, Intermedio) + b(Intermedio, T, E)),
												   crear_transicion('¬', Primer, Intermedio), crear_transicion(T, Intermedio, E), insert_trans(Primer, Xr, Intermedio).


retract_trans(X, []) :- \+transicion(X, T, E), write("Después retract_trans\n").
retract_trans(X, [[E, T]|L1]) :- transicion(X, T, E), write("retract_trans: " + transicion(X, T, E) + "\n"), retract(transicion(X, T, E)), retract_trans(X, L1), write(L1 + "\n").

nuevo_estado(Nuevo) :- ultimo_estado(E), retract(ultimo_estado(E)), Nuevo is E + 1, 
					   assert(ultimo_estado(Nuevo)), assert(estado(Nuevo)).
