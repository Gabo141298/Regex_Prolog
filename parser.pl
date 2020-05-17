% regex_match/3(+ExpresiónRegular,+Hilera,-Match): 
%				Busca una subhilera en la hilera donde se encuentre la expresión regular.
%				Parámetros:
%					ExpresiónRegular: expresión regular que se desea encontrar.
%					Hilera: hilera donde se desea encontrar la expresión regular.
%					Match: regresa la subhilera que cumple con la expresión regular.
%				Ejemplos:
%					- regex_match("ab|cd","abcw",A) devuelve en A el string "ab".
%					- regex_match("a","b",B) falla.
regex_match(Regex, Hilera, Match) :- retract_all(),
									 string_chars(Regex, RegexChars),
									 parse_regex(RegexChars),
									 string_chars(Hilera, HileraChars),
									 estado_inicial(X),
									 accept_string(HileraChars, MatchChars, X),
									 !, estado_aceptado,
									 string_chars(Match, MatchChars).

% retract_all/1:
%				Se encarga de hacer retract a todos los asserts que se hacen en el programa.
retract_all() :- retractall(estado(_)),
				 retractall(estado_aceptacion(_)),
				 retractall(ultimo_estado(_)),
				 retractall(transicion(_, _, _)),
				 retractall(estado_aceptado).

% accept_string/3(+Hilera, -Match, +Actual):
%				Usa las reglas que forman al automata para verificar si el automata acepta a la hilera.
%				Parámetros:
%					Hilera: lista con cada caracter de la hilera.
%					Match: lista donde se devuelve la subhilera que es aceptada por el automata.
%					Actual: estado actual que se está recorriendo en el automata.
accept_string(_, [], Actual) :- estado_aceptacion(Actual), assert(estado_aceptado), !.
accept_string([], Match, Actual) :- findall(Y, transicion(Actual, '¬', Y), L),
									explore_transitions([], '¬', Actual, L, Match).
accept_string([], [], _).
accept_string([X|Xr], Match, Actual) :- findall(Y, transicion(Actual, X, Y), L1),
										findall(Z, transicion(Actual, '¬', Z), L2),
										(explore_transitions([X|Xr], X, Actual, L1, Match);
										explore_transitions([X|Xr], '¬', Actual, L2, Match)).
accept_string([_|Xr], Match, _) :- estado_inicial(I), accept_string(Xr, Match, I).

% explore_transitions/5(+Hilera, +CaracterTransicion, +Actual, +Transiciones, -Match):
%				Explora todas las transiciones que hay desde el estado actual del automata usando un caracter de transición en particular.
%				Parámetros:
%					Hilera: lista con caracteres de la hilera.
%					CaracterTransicion: caracter con el que se está haciendo la transición. Principalmente usado para verificar si es un nil (¬).
%					Actual: estado actual que se está recorriendo en el automata.
%					Transiciones: lista con los posibles estados a los que se puede pasar desde el estado actual, con el caracter de transición.
%					Match: lista donde se devuelve la subhilera que es aceptada por el automata.
explore_transitions(_, _, Actual, _, []) :- estado_aceptacion(Actual), !.
explore_transitions(Hilera, '¬', _, [Y|_], Match) :- accept_string(Hilera, Match, Y), estado_aceptado.
explore_transitions(Hilera, '¬', Actual, [_|Yr], Match) :- explore_transitions(Hilera, '¬', Actual, Yr, Match), !.
explore_transitions([X|Xr], _, _, [Y|_], [X|Match]) :- accept_string(Xr, Match, Y), estado_aceptado.
explore_transitions([X|Xr], _, Actual, [_|Yr], [X|Match]) :- explore_transitions([X|Xr], X, Actual, Yr, Match).

% parse_regex/1(+Regex):
%				parsea la expresión regular, para generar las reglas para los estados y las transiciones.
%				Parámetros:
%					Regex: lista con los caracteres de la expresión regular.
parse_regex(Regex) :- assert(estado(0)), assert(estado_inicial(0)), assert(estado(1)), assert(ultimo_estado(1)), 
					  assert(estado_aceptacion(1)), parse(Regex, [[0,1]], [0, 0]).

% parse/3(+Regex, +EstadosAfueraParentesis, +UltimosEstados):
%				parte principal del parseo de la expresión regular. Lleva cuenta del anidamiento por paréntesis
%				y por dónde va en la expresión regular.
%				Parámetros:
%					Regex: lista con los caracteres que describen a la expresión regular.
%					EstadosAfueraParentesis: lista con los pares de estados que encierran a la expresión que está entre paréntesis.
%					UltimosEstados: lista con los dos estados anteriores en la expresión, con orden [Ultimo, Penultimo].
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

% crear_transicion/3(+Caracter, +EstadoInicial, +EstadoFinal):
%				verifica si existe la transición que se desea crear, si no, la crea.
%				Parámetros:
%					Caracter: caracter de transición entre estados.
%					EstadoInicial: estado desde donde se hace la transición.
%					EstadoFinal: estado a donde se llega por medio de la transición.
crear_transicion(Caracter, EstadoInicial, EstadoFinal) :- transicion(EstadoInicial, Caracter, EstadoFinal).
crear_transicion(Caracter, EstadoInicial, EstadoFinal) :- assert(transicion(EstadoInicial, Caracter, EstadoFinal)).


% estrella/3(+PrimerEstado, +UltimoEstado, -NuevoUltimoEstado):
%				crea los estados y transiciones para generar estrella desde un primer estado hasta un último estado.
%				Parámetros:
%					PrimerEstado: estado donde inicia la expresión a la que se le aplica estrella.
%					UltimoEstado: estado donde termina la expresión a la que se le aplica estrella.
%					NuevoUltimoEstado: se devuelve el nuevo estado final de la expresión.
% [[PrimerEstado,nil,Intermedio],[Intermedio,X,UltimoEstado],[UltimoEstado,nil,NuevoUltimoEstado],
% [PrimerEstado,nil,NuevoUltimoEstado],[NuevoUltimoEstado,nil,Intermedio]]
estrella(PrimerEstado, UltimoEstado, NuevoUltimoEstado) :- nuevo_estado(Intermedio),
														   nuevo_estado(NuevoUltimoEstado),
														   retract_trans(PrimerEstado, L),
														   insert_trans(PrimerEstado, L, Intermedio),
														   crear_transicion('¬', UltimoEstado, NuevoUltimoEstado),
														   crear_transicion('¬', PrimerEstado, NuevoUltimoEstado),
														   crear_transicion('¬', NuevoUltimoEstado, Intermedio).

% insert_trans/3(+PrimerEstado, +EstadosSiguientes, +Intermedio):
%				Inserta un estado en medio de un primer estado y todos los posibles estados a los que puede llegar directamente.
%				Parámetros:
%					PrimerEstado: estado al que se le pone una transición con el estado intermedio.
%					EstadosSiguientes: lista de estados a los que se podía llegar por medio del primer estado.
%					Intermedio: estado que se mete en medio del primer estado y los estados siguientes.
insert_trans(_, [], _).
insert_trans(Primer, [[E, T] | Xr], Intermedio) :- crear_transicion('¬', Primer, Intermedio), crear_transicion(T, Intermedio, E), 
												   insert_trans(Primer, Xr, Intermedio).

% retract_trans/2(+EstadoInicial, -EstadosSiguientes):
%				Quita de la base de conocimientos a las transiciones que iban desde un estado inicial a otros y guarda esos otros estados.
%				Parámetros:
%					EstadoInicial: estado al que se le quitan todas sus transiciones.
%					EstadosSiguientes: lista que se retorna con todos los estados a los que podía llegar por medio de EstadoInicial.
retract_trans(X, []) :- \+transicion(X, _, _).
retract_trans(X, [[E, T]|L1]) :- transicion(X, T, E), retract(transicion(X, T, E)), retract_trans(X, L1).

% nuevo_estado/1(-Nuevo):
%				Crea un nuevo estado.
%				Parámetros:
%					Nuevo: valor numérico donde se retorna el valor del nuevo estado.
nuevo_estado(Nuevo) :- ultimo_estado(E), retract(ultimo_estado(E)), Nuevo is E + 1, 
					   assert(ultimo_estado(Nuevo)), assert(estado(Nuevo)).
