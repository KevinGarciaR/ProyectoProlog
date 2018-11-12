/* PROLOG - 08:00 - 09:00
Integrantes:
	Kevin Jesus Garcia Rios
	Ivan Alan Armenta Bernal
	Jesus Lowell Quiñonez Rojo
*/

:-dynamic
	pokebolasJugador/1,
	huevosJugador/1.

%Nombre, costo, probabilidad de atrapar el Pokemon
pokebolas([[normal,30,40],[azul,60,60],[negra,100,90]]).

%pokemones
%Nombre, tipo, salud, ataques, estado, nivel, experiencia
pokemones([
		  [pikachu,  electrico,100,[ impactrueno,   cabezazo,	tacleada,	chispa],    excelente, 1, 0],
		  [electabuzz,electrico,100,[impactrueno,   amago,      grunido,    chispa],    excelente, 1, 0],
          [voltorb,  electrico,100,[ agilidad,      destello,   impactrueno,chispa],	excelente, 1, 0],

		  [squirtle, agua,100,[burbuja,   cabezazo,     mordisco,         latigo],		excelente, 1, 0],
		  [croconaw, agua,100,[giro_rapido, placaje,    latigo,           hidropulso],   excelente, 1, 0],
		  [krabby,   agua,100,[hidropulso,danza ,	    hidrobomba,       burbuja	 ], excelente, 1, 0],

		  [charmander,fuego,100,[aranazo,  garra_umbria,	pantalla_de_Humo, cuchilla ],     excelente, 1, 0],
		  [magmar,    fuego,100,[infierno, lanzallamas,     giro_fuego,       grunido],       excelente, 1, 0],
		  [moltres,   fuego,100,[aranazo,  grunido,         cuchilla,         hidropulso],    excelente, 1, 0],
		 
		  [ratata,  normal,100,[esfuerzo, doble_filo,   golpe_bajo,       superdiente],   excelente, 1, 0],
		  [snorlax, normal,100,[esfuerzo, persecucion,  foco_energia,     triturar],      excelente, 1, 0],
		  [ursaring,normal,100,[finta,    llanto_falso, foco_energia,     ronquido],      excelente, 1, 0],

		  [rhyhorn, tierra,100,[cornada,  taladradora,  pisoton,          terremoto ],    excelente, 1, 0],
		  [cubone,  tierra,100,[derribo,  antiaereo,    roca_afilada,     megacuerno],    excelente, 1, 0],
		  [diglett, tierra,100,[pisoton,  taladradora,  roca_afilada,     derribo],       excelente, 1, 0]
		  ]).

%Ataques
habilidad(cornada,15).
habilidad(taladradora,15).
habilidad(pisoton,10).
habilidad(terremoto,10).
habilidad(derribo,20).
habilidad(antiaereo,20).
habilidad(roca_afilada,15).
habilidad(megacuerno,15).
habilidad(finta,10).
habilidad(llanto_falso,15).
habilidad(ronquido,20).
habilidad(persecucion,8).
habilidad(foco_energia,10).
habilidad(triturar,20).
habilidad(esfuerzo,5).
habilidad(doble_filo,15).
habilidad(golpe_bajo,30).
habilidad(superdiente,10).
habilidad(aranazo,12).
habilidad(garra_umbria,20).
habilidad(pantalla_de_Humo,19).
habilidad(cuchilla,30).
habilidad(infierno,26).
habilidad(lanzallamas,20).
habilidad(giro_rapido,8).
habilidad(grunido,6).
habilidad(burbuja,6).
habilidad(hidrobomba,30).
habilidad(danza,8).
habilidad(hidropulso,20).
habilidad(placaje,8).
habilidad(giro_rapido,5).
habilidad(impactrueno,10).
habilidad(cabezazo,25).
habilidad(tacleada,15).
habilidad(chispa,5).
habilidad(amago,10).
habilidad(grunido,15).
habilidad(agilidad,5).
habilidad(destello,15).
habilidad(mordisco,20).
habilidad(latigo,25).


evolucion(pikachu, raichu,4).
evolucion(squirtle, wartortle,2).
 evolucion(wartortle, blastoise,3).
evolucion(charmander, charmeleon,2).
 evolucion(charmeleon, charizard,4).
evolucion(ratata, raticate,3).
evolucion(cubone, marowak,2).
evolucion(voltorb, electrode,2).

%Tipo, distancia para nacer
huevos([[electrico,80],[agua,60],[fuego,90],[normal,30],[tierra,50]]).
ciudades([culiacan, mazatlan, guadalajara, monterrey, tepic, puebla]).
% ciudad origen, ciudad destino, distancia
distancia(culiacan,mazatlan,50).
distancia(culiacan,guadalajara,70).
distancia(culiacan,monterrey,60).
distancia(culiacan,tepic,40).
distancia(culiacan,puebla,30).
distancia(mazatlan,guadalajara,40).
distancia(mazatlan,monterrey,50).
distancia(mazatlan,tepic,50).
distancia(mazatlan,puebla,20).
distancia(guadalajara,monterrey,30).
distancia(guadalajara,tepic,10).
distancia(guadalajara,puebla,40).
distancia(monterrey,tepic,30).
distancia(monterrey,puebla,50).
distancia(tepic,puebla,30).
% invertir distancias
distancia2(Ciudad1,Ciudad2,Distancia):-
  distancia(Ciudad1,Ciudad2,Distancia).
distancia2(Ciudad1,Ciudad2,Distancia):-
  distancia(Ciudad2,Ciudad1,Distancia).
% estado del jugador
dinerojugador(0).
pokebolasJugador([[normal,30,40],[azul,60,60],[negra,100,90]]).
pokemonesJugador([pikachu,  electrico,100,[ impactrueno,   cabezazo,	tacleada,	chispa],    excelente, 1, 0]).
huevosJugador([]). %Huevos Iniciales

juegoPokemon:-
  caminarASiguienteCiudad.
  %llegasASiguienteCiudad.

caminarASiguienteCiudad:-
  %preguntarCiudad(CiudadParaIr),
  %random(1,4, Random),
  posibilidadAlCaminar(2).
  /*cambiarCiudad(CiudadParaIr).*/

  /*Encontro pokebola*/
posibilidadAlCaminar(1):-
	write("Has encontrado una pokebola en tu trayecto."),nl,
	pokebolas(Pokebolas), %unificacion las lista de pokebolas a la variable
	length(Pokebolas,Longitud),
	random(1,Longitud, Random),
	sacarElementoDeLista(Random,Pokebolas,Pokebola),
	agregarPokebolaALaMochila(Pokebola).

%Encontro un HUEVO
posibilidadAlCaminar(2):-
write("Has encontrado un Huevo en tu trayecto."),nl,
	huevos(Huevos), %unificacion las lista de huevos a la variable
	length(Huevos,Longitud),
	random(1,Longitud, Random),
	sacarElementoDeLista(Random,Huevos,Huevo),
	hayEspacio,
	agregarHuevoALaMochila(Huevo).
posibilidadAlCaminar(2):-
	preguntarGuardadoHuevos(Respuesta), % Preguntar se se conservara huevo o se enviara a Bill
    posibilidadConHuevo(Respuesta,Huevo).
posibilidadConHuevo(si,Huevo).
    preguntarQuePokemonVasaSacar(Indice),
   	mandarPokemonABill(Indice).

posibilidadConHuevo(si,Huevo):-
	.


sacarElementoDeLista(Indice,Lista,Elemento):-sacarElementoDeLista(Indice,1,Lista,Elemento).
sacarElementoDeLista(Indice,Contador,Lista,Elemento):-
	Lista = [Cabeza|Cola],
	Indice=Contador,
	Elemento=Cabeza.

sacarElementoDeLista(Indice,Contador,Lista,Elemento):-
	Lista = [Cabeza|Cola],
	Indice\=Contador,
	NuevoContador is Contador+1,
	sacarElementoDeLista(Indice,NuevoContador,Cola,Elemento).

agregarPokebolaALaMochila(NuevaPokebola):-
   pokebolasJugador(Pokebolas),
   retractall(pokebolasJugador(Pokebolas)),
   assert(pokebolasJugador([NuevaPokebola|Pokebolas])),
   write("Se agrego exitosamente la pokebola a la mochila"),nl.

agregarHuevoALaMochila(NuevoHuevo):-
	huevosJugador(Huevos),
	retractall(huevosJugador(Huevos)),
	assert(huevosJugador([NuevoHuevo|Huevos])),
	write("Se agrego exitosamente el huevo a la mochila"),nl.

preguntarCiudad(CiudadParaIr):-
	write("Elige una ciudad para ir:"),nl,
	ciudades(X),
	write(X),nl,
	read(CiudadParaIr),
	estaDentro(CiudadParaIr,X).

preguntarCiudad(CiudadParaIr):-
	write("No es valida esa ciudad"),nl,
	preguntarCiudad(CiudadParaIr).


%Saber si un elemento esta dentro de una lista
estaDentro(Elemento,[Elemento|Cola]).
estaDentro(Elemento,[Cabeza|Cola]):-
	estaDentro(Elemento,Cola).

%Hacer una regla que sea verdadera. 
hayEspacio:-
	pokemonesJugador(PokemonesJugador),
	huevosJugador(HuevosJugador),
	length(HuevosJugador,LongitudHuevos),
	length(Pokemonesjugador,LongitudPokemones),
	Suma is LongitudHuevos + LongitudPokemones,
	Suma <6.

preguntarGuardadoHuevos(Respuesta):-
	write("Te guardo el huevo en la Mochila? o se enviara a Bill (si/no)"),nl,
	read(Respuesta),
	estaDentro(Respuesta,[si,no]).
preguntarGuardadoHuevos(Respuesta):-
	write("No es valida esa respuesta"),nl,
	preguntarGuardadoHuevos(Respuesta).

preguntarQuePokemonVasaSacar(Respuesta):-
     write("¿Que pokemon vas a sacar? Pon un indice"),nl,
     pokemonesJugador(Pokemones),
     length(Pokemones,LongitudPokemones)
     write(Pokemones),nl,
	 read(Respuesta),
	 Respuesta =<LongitudPokemones, 
	 Respuesta>=1. 
	
mandarPokemonABill(Indice):-
  	pokemonesJugador(ListaPokemonesJugador),
	sacarElementoDeLista(Indice,ListaPokemonesJugador,PokemonEnviadoABill).
listaSinElemento(Indice,Lista,ListaSinElemento):-listaSinElemento(Indice,1,Lista,ListaSinElemento).
listaSinElemento(Indice,Contador,_,_)
listaSinElemento(Indice,Contador,Lista,NuevaLista,[Cabeza|NuevaLista]):-
	Lista = [Cabeza|Cola],
	Indice\=Contador,
	NuevoContador is Contador+1,
	listaSinElemento(Indice,NuevoContador,NuevaLista,[Cabeza|NuevaLista],)

	

listaSinElemento(Indice,Contador,Lista,Elemento):-
	Lista = [Cabeza|Cola],
	Indice\=Contador,
	NuevoContador is Contador+1,
	listaSinElemento(Indice,NuevoContador,Cola,Elemento).