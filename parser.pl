% Tarea 1 - Teoría de la computación
% - Christian Asch
% - Gabriel Gálvez
% - Christian Rodríguez

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
									 %listing(transicion),
									 string_chars(Hilera, HileraChars),
									 estado_inicial(X),
									 assert(hilera_actual(HileraChars)),
									 accept_string(HileraChars, MatchChars, X, []),
									 !, estado_aceptado,
									 string_chars(Match, MatchChars).

% retract_all/1:
%				Se encarga de hacer retract a todos los asserts que se hacen en el programa.
retract_all() :- retractall(estado(_)),
				 retractall(estado_aceptacion(_)),
				 retractall(ultimo_estado(_)),
				 retractall(transicion(_, _, _)),
				 retractall(estado_aceptado).

% accept_string/4(+Hilera, -Match, +Actual, +UltimosRecorridosConNil):
%				Usa las reglas que forman al automata para verificar si el automata acepta a la hilera.
%				Parámetros:
%					Hilera: lista con cada caracter de la hilera.
%					Match: lista donde se devuelve la subhilera que es aceptada por el automata.
%					Actual: estado actual que se está recorriendo en el automata.
%					UltimosRecorridosConNil: lista con los últimos estados que se recorrieron con nil, 
%											para evitar recorrer los mismos estados y enciclar el programa.
accept_string(_, [], Actual, _) :- estado_aceptacion(Actual), assert(estado_aceptado), !.
accept_string([], Match, Actual, RecorridosNil) :- findall(Y, transicion(Actual, '¬', Y), L),
									explore_nil_transitions([], Actual, L, Match, RecorridosNil).
accept_string([], [], _, _).
accept_string([X|Xr], Match, Actual, RecorridosNil) :- findall(Y, transicion(Actual, X, Y), L1),
										findall(Z, transicion(Actual, '¬', Z), L2),
										(explore_transitions([X|Xr], Actual, L1, Match);
										explore_nil_transitions([X|Xr], Actual, L2, Match, RecorridosNil)).
accept_string([X|Xr], Match, _, _) :- hilera_actual([X|Xr]), retract(hilera_actual([X|Xr])), assert(hilera_actual(Xr)),
									estado_inicial(I), accept_string(Xr, Match, I, []), !.

% explore_transitions/4(+Hilera, +Actual, +Transiciones, -Match):
%				Explora todas las transiciones que hay desde el estado actual del automata usando un caracter de transición en particular.
%				Parámetros:
%					Hilera: lista con caracteres de la hilera.
%					Actual: estado actual que se está recorriendo en el automata.
%					Transiciones: lista con los posibles estados a los que se puede pasar desde el estado actual, con el caracter de transición.
%					Match: lista donde se devuelve la subhilera que es aceptada por el automata.
explore_transitions(_, Actual, _, []) :- estado_aceptacion(Actual), !.
explore_transitions([X|Xr], _, [Y|_], [X|Match]) :- accept_string(Xr, Match, Y, []), estado_aceptado.
explore_transitions([X|Xr], Actual, [_|Yr], [X|Match]) :- explore_transitions([X|Xr], Actual, Yr, Match), estado_aceptado.

explore_nil_transitions(_, Actual, _, [], _) :- estado_aceptacion(Actual), !.
explore_nil_transitions(Hilera, _, [Y|_], Match, RecorridosNil) :- \+member(Y, RecorridosNil), 
																   accept_string(Hilera, Match, Y, [Y|RecorridosNil]), estado_aceptado.
explore_nil_transitions(Hilera, Actual, [_|Yr], Match, RecorridosNil) :- explore_nil_transitions(Hilera, Actual, Yr, Match, RecorridosNil), estado_aceptado.

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
parse([')','*'|Xr], [[AbreEstado, CierraEstado] | EstadosAfuera], [UltimoEstado|_]):- 
																	crear_transicion('¬', UltimoEstado, CierraEstado),
																	%listing(transicion), write(AbreEstado + CierraEstado + NuevoUltimoEstado + "\n"),
																	estrella(AbreEstado, CierraEstado, NuevoUltimoEstado), 
																 	parse(Xr, EstadosAfuera, [NuevoUltimoEstado, CierraEstado]).
parse([')'|Xr], [[_, CierraEstado]|EstadosAfuera], [UltimoEstado, _]) :- crear_transicion('¬', UltimoEstado, CierraEstado),
																	   parse(Xr, EstadosAfuera, [CierraEstado, UltimoEstado]).
parse(['('|Xr], [[AbreEstado, CierraEstado] | EstadosAfuera], [UltimoEstado, _]) :- nuevo_estado(Nuevo),
													 crear_transicion('¬', AbreEstado, Nuevo),
													 nuevo_estado(NuevoCierre), 
													 parse(Xr, [[UltimoEstado, NuevoCierre], [AbreEstado, CierraEstado] | EstadosAfuera], [Nuevo, UltimoEstado]).
parse(['|'|Xr], [[AbreEstado, CierraEstado] | EstadosAfuera], [UltimoEstado, _]) :- %write([AbreEstado, CierraEstado] + UltimoEstado + "\n"),
																					nuevo_estado(Nuevo),
																					crear_transicion('¬', AbreEstado, Nuevo),
																					%listing(transicion),
																					crear_transicion('¬', UltimoEstado, CierraEstado),
																					parse(Xr, [[AbreEstado, CierraEstado] | EstadosAfuera], [Nuevo, AbreEstado]).
parse(['*'|Xr], EstadosAfuera, [UltimoEstado, PenultimoEstado]) :- %write("estrella sola: " + [UltimoEstado, PenultimoEstado]),
																   %listing(transicion),
																   estrella(PenultimoEstado, UltimoEstado, NuevoUltimoEstado),
																   %listing(transicion),
																   parse(Xr, EstadosAfuera, [NuevoUltimoEstado, UltimoEstado]).
parse([X|Xr], EstadosAntesParen, [UltimoEstado, _]) :- %write("Carac: " + X + EstadosAntesParen + UltimoEstado + "\n"),
													   nuevo_estado(Trans), crear_transicion(X, UltimoEstado, Trans), 
													   %listing(transicion),
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
