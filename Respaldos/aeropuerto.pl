aeropuerto(1,[2,5]).
aeropuerto(5,[6]).
aeropuerto(6,[8]).
aeropuerto(2,[7]).

conectados(X,DESTINO):-
	aeropuerto(X,ALGO),
	member(DESTINO,ALGO).
	

impresionLista(Lista):-
	Lista=[Cabeza|Cola],
	write(Cabeza),nl,
	impresionLista(Cola).
impresionLista([]).


conectadosIndirecto(X,DESTINO):-
	write(X),nl,
	conectados(X,DESTINO).

conectadosIndirecto(X,DESTINO):-
	write(X),nl,
	aeropuerto(X,ALGO),
	member(Y,ALGO),
	conectadosIndirecto(Y,DESTINO).


