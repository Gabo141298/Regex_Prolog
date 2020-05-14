regex_match(Regex, Hilera, Match) :- string_chars(Regex, RegexChars),
									 parse_regex(RegexChars).

parse_regex(Regex) :- parse(Regex, [0], 0).

parse([], _, _).
parse([')','*'|Xr], [PrimerEstado | EstadosAfuera], UltimoEstado):- estrella(PrimerEstado, UltimoEstado, NuevoUltimoEstado), 
																 	parse(Xr, EstadosAfuera, NuevoUltimoEstado).
parse([')'|Xr], [_|EstadosAfuera], UltimoEstado) :- parse(Xr, EstadosAfuera, UltimoEstado).
parse(['('|Xr], EstadosAfuera, UltimoEstado) :- parse(Xr, [UltimoEstado | EstadosAfuera], UltimoEstado).
parse(['|'|Xr], [PrimerEstado|EstadosAfuera], _) :- parse(Xr, [PrimerEstado|EstadosAfuera], PrimerEstado).
parse(['*'|Xr], _, UltimoEstado) :- estrella(UltimoEstado, UltimoEstado, NuevoUltimoEstado), 
									parse(Xr, EstadosAfuera, NuevoUltimoEstado).
parse([X|Xr], EstadosAntesParen, UltimoEstado) :- crear_transicion(X, UltimoEstado, Trans), write(transicion(UltimoEstado, X, Trans)), parse(Xr, EstadosAntesParen, Trans).

crear_transicion(Caracter, EstadoInicial, EstadoFinal) :- nuevo_estado(EstadoInicial, EstadoFinal), assert(transicion(EstadoInicial, Caracter, EstadoFinal)).

estrella(PrimerEstado, UltimoEstado, NuevoUltimoEstado).

nuevo_estado(Anterior, Nuevo) :- Nuevo is Anterior + 1, assert(estado(S)).
