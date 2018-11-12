/* PROLOG - 08:00 - 09:00
Integrantes:
	Kevin Jesus Garcia Rios
	Ivan Alan Armenta Bernal
	Jesus Lowell Quiñonez Rojo
*/

:-dynamic
	pokebolasJugador/1,
	huevosJugador/1,
	pokemonesJugador/1,
	pokemonesPeleando/2,
	pokemonesconBill/1.



%Pokebolas y huevos iniciales
%Nombre, costo, probabilidad de atrapar el Pokemon
pokebolas([[normal,30,40],[azul,60,60],[negra,100,90]]).
%Tipo, distancia para nacer
huevos([[electrico,80],[agua,60],[fuego,90],[normal,30],[tierra,50]]).

%pokemones
%Nombre, tipo, salud, ataques, estado, nivel, experiencia
pokemones([
		  [pikachu,  electrico,100,[ impactrueno,   cabezazo,	tacleada,	chispa],    vivo, 1, 0],
		  [electabuzz,electrico,100,[impactrueno,   amago,      grunido,    chispa],    vivo, 1, 0],
          [voltorb,  electrico,100,[ agilidad,      destello,   impactrueno,chispa],	vivo, 1, 0],

		  [squirtle, agua,100,[burbuja,   cabezazo,     mordisco,         latigo],		vivo, 1, 0],
		  [croconaw, agua,100,[giro_rapido, placaje,    latigo,           hidropulso],   vivo, 1, 0],
		  [krabby,   agua,100,[hidropulso,danza ,	    hidrobomba,       burbuja	 ], vivo, 1, 0],

		  [charmander,fuego,100,[aranazo,  garra_umbria,	pantalla_de_Humo, cuchilla ],     vivo, 1, 0],
		  [magmar,    fuego,100,[infierno, lanzallamas,     giro_fuego,       grunido],       vivo, 1, 0],
		  [moltres,   fuego,100,[aranazo,  grunido,         cuchilla,         hidropulso],    vivo, 1, 0],
		 
		  [ratata,  normal,100,[esfuerzo, doble_filo,   golpe_bajo,       superdiente],   vivo, 1, 0],
		  [snorlax, normal,100,[esfuerzo, persecucion,  foco_energia,     triturar],      vivo, 1, 0],
		  [ursaring,normal,100,[finta,    llanto_falso, foco_energia,     ronquido],      vivo, 1, 0],

		  [rhyhorn, tierra,100,[cornada,  taladradora,  pisoton,          terremoto ],    vivo, 1, 0],
		  [cubone,  tierra,100,[derribo,  antiaereo,    roca_afilada,     megacuerno],    vivo, 1, 0],
		  [diglett, tierra,100,[pisoton,  taladradora,  roca_afilada,     derribo],       vivo, 1, 0]
		  ]).

%Ataques NombreAtaque,Total de daño

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
habilidad(impactrueno,100).
habilidad(cabezazo,25).
habilidad(tacleada,15).
habilidad(chispa,5).
habilidad(amago,10).
habilidad(grunido,15).
habilidad(agilidad,5).
habilidad(destello,15).
habilidad(mordisco,20).
habilidad(latigo,25).

%PokemonInicial, PokemonEvolucionado, Nivel que ocupa para evolucionar -> Dependiendo la experiencia
evolucion(pikachu, raichu,4).
evolucion(squirtle, wartortle,2).
evolucion(wartortle, blastoise,2).
evolucion(charmander, charmeleon,3).
evolucion(charmeleon, charizard,4).
evolucion(ratata, raticate,3).
evolucion(cubone, marowak,2).
evolucion(voltorb, electrode,2).

%Ciudades que existen (6)
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
pokemonesconBill([]).
pokemonesJugador([[pikachu,  electrico,100,[ impactrueno,   cabezazo,	tacleada,	chispa],    vivo, 1, 0],
				[voltorb,  electrico,100,[ agilidad,      destello,   impactrueno,chispa],	vivo, 1, 0]]).
huevosJugador([[electrico,80],[agua,60],[fuego,90],[normal,30],[tierra,50]]). %Huevos Iniciales

juegoPokemon:-
  caminarASiguienteCiudad.
  %llegasASiguienteCiudad.

caminarASiguienteCiudad:-
  %preguntarCiudad(CiudadParaIr),
  random(1,3, Random),
  posibilidadAlCaminar(3).

  %cambiarCiudad(CiudadParaIr).

  /*Encontro pokebola*/
posibilidadAlCaminar(1):-
	write("Has encontrado una pokebola en tu trayecto."),nl,
	pokebolas(Pokebolas), %unificacion las lista de pokebolas a la variable
	length(Pokebolas,Longitud),
	random(0,Longitud, Random),
	sacarElementoDeLista(Random,Pokebolas,Pokebola),
	agregarPokebolaALaMochila(Pokebola).

%Encontro un HUEVO
posibilidadAlCaminar(2):-
write("Has encontrado un Huevo en tu trayecto."),nl,
	huevos(Huevos), %unificacion las lista de huevos a la variable
	length(Huevos,Longitud),
	random(0,Longitud, Random),
	sacarElementoDeLista(Random,Huevos,Huevo),
	hayEspacio,
	agregarHuevoALaMochila(Huevo).

posibilidadAlCaminar(2):-
	preguntarGuardadoHuevos(Respuesta), % Preguntar se se conservara huevo o se enviara a Bill
    posibilidadConHuevo(Respuesta,Huevo).

posibilidadConHuevo(Respuesta,Huevo):-
    preguntarQuePokemonVasaSacar(Indice),
   	mandarPokemonABill(Indice).

posibilidadAlCaminar(3):-
	sacarPokemonRandom(Pokemon),
	Pokemon= [Cabeza|_],
	write("Has encontrado al pokemon: "), write(Cabeza), write(" deseas pelear con el? (si/no)"), nl,
	read(Respuesta),
	respuestaSobrePelea(Respuesta,Pokemon, Cabeza).

respuestaSobrePelea(no,Pokemon,_):-
write("Te perdiste de un gran Pokemon :(").

respuestaSobrePelea(si,PokemonEnemigo, NombreEnemigo):-
write("Preparado?"),nl,
write("Necesitas escoger a un Pokemon para enfrentar a "), write(NombreEnemigo),nl,
    pokemonesJugador(Pokemones),
	length(Pokemones,LongitudPokemones),
	impresionListaNumerada(Pokemones),nl,
	read(Indice),
	sacarElementoDeLista(Indice,Pokemones,TuPokemon),
	peleaPokemones(TuPokemon, PokemonEnemigo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
peleaPokemones(Mipokemon,Pokemonenemigo):-
	ponganseaPelear(Mipokemon,Pokemonenemigo),
	pokemonesPeleando(MiPokemon1,PokemonEnemigo1),
	ponganseaPelear(MiPokemon1,PokemonEnemigo1),
	pokemonesPeleando(MiPokemon2,PokemonEnemigo2),
	ponganseaPelear(MiPokemon2,PokemonEnemigo2),
	pokemonesPeleando(MiPokemon3,PokemonEnemigo3),
	ponganseaPelear(MiPokemon3,PokemonEnemigo3),
	pokemonesPeleando(MiPokemon4,PokemonEnemigo4),
	write(" Mi pokemon final:"),write(MiPokemon4),nl,
	write(" El pokemon enemigo final:"),write(PokemonEnemigo4),nl,
	quienGano(Mipokemon,MiPokemon4,PokemonEnemigo4). %%%%%%%%%

peleaPokemones(Mipokemon,Pokemonenemigo):-
	pokemonesPeleando(MiPokemon4,PokemonEnemigo4),
	write(" Mi pokemon final:"),write(MiPokemon4),nl,
	write(" El pokemon enemigo final:"),write(PokemonEnemigo4),nl,
	quienGano(Mipokemon,MiPokemon4,PokemonEnemigo4). 
peleaPokemones(_,_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



actualizarPokemon(Mipokemon,MiPokemonFinal):-
pokemonesJugador(Pokemones),
actualizarEstadoDePokemon(MiPokemonFinal,MiPokemonActualizado ),


nth0(IndiceResultado,Pokemones,MiPokemon),
reemplazarenIndice(Pokemones, IndiceResultado, MiPokemonActualizado, NuevaLista),
retractall(pokemonesJugador(_)),
assert(pokemonesJugador(NuevaLista)).

actualizarEstadoDePokemon(Pokemon, PokemonNuevo):-
	Pokemon=[Nombre,Tipo,Salud,Ataques,Estado,Nivel,Experiencia],
	Salud=<0,
	PokemonNuevo=[Nombre,Tipo,0,Ataques,muerto,Nivel,Experiencia].	


actualizarEstadoDePokemon(Pokemon, PokemonNuevo):-
	Pokemon=[Nombre,Tipo,Salud,Ataques,Estado,Nivel,Experiencia],
	Salud<60,
	PokemonNuevo=[Nombre,Tipo,Salud,Ataques,critico,Nivel,Experiencia].

actualizarEstadoDePokemon(Pokemon, PokemonNuevo):-
	Pokemon=[Nombre,Tipo,Salud,Ataques,Estado,Nivel,Experiencia],
	PokemonNuevo=[Nombre,Tipo,Salud,Ataques,vivo,Nivel,Experiencia].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
actualizarExperienciaPokemon(Pokemon, PokemonNuevo):-	%si sumatoria de experiencia supera 100 se aumenta el nivel y exériencia=0
	Pokemon=[Nombre,Tipo,Salud,Ataques,Estado,Nivel,Experiencia],
	NuevaExperiencia is Experiencia + 35,
	NuevaExperiencia>=100,
	NuevoNivel is Nivel+1,
	PokemonNuevo=[Nombre,Tipo,Salud,Ataques,Estado,NuevoNivel,0].	

actualizarExperienciaPokemon(Pokemon, PokemonNuevo):- %Si la sumatoria de la experiencia adquirida no es mayor a 100 entra a aqui
	Pokemon=[Nombre,Tipo,Salud,Ataques,Estado,Nivel,Experiencia],
	NuevaExperiencia is Experiencia + 35,
	PokemonNuevo=[Nombre,Tipo,Salud,Ataques,Estado,Nivel,NuevaExperiencia].	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		

quienGano(PokemonInicial,MiPokemonFinal,PokemonEnemigoFinal):-
 MiPokemonFinal=[NombreMiPokemon,_,SaludFinal|_],
 PokemonEnemigoFinal=[_,_,SaludEnemigoFinal|_],
 SaludFinal > SaludEnemigoFinal,
 actualizarExperienciaPokemon(MiPokemonFinal, MiPokemonFinalActualizado),
 actualizarPokemon(Mipokemon,MiPokemonFinalActualizado),

 write("Gano mi Pokemon "), write(NombreMiPokemon),nl.

 quienGano(_,MiPokemonFinal,PokemonEnemigoFinal):-
 MiPokemonFinal=[_,_,SaludFinal|_],
 PokemonEnemigoFinal=[NombrePokemonEnemigo,_,SaludEnemigoFinal|_],
 SaludFinal < SaludEnemigoFinal,
  actualizarPokemon(Mipokemon,MiPokemonFinal),

 write("Gano el pokemon enemigo, sigue entrenando a tu Pokemon"), write(NombrePokemonEnemigo),nl.

 quienGano(_,MiPokemonFinal,PokemonEnemigoFinal).


respuestaSobrePelea(si,PokemonEnemigo, NombreEnemigo):-respuestaSobrePelea(si,PokemonEnemigo, NombreEnemigo).

sacarPokemonRandom(Pokemon):-
 	pokemones(PokemonPeleador),
	length(PokemonPeleador, LongitudPokemonesP),
	random(0,LongitudPokemonesP, Random),
	sacarElementoDeLista(Random,PokemonPeleador,Pokemon).

respuestaSobrePelea(Respuesta):-
	read(Respuesta),
	estaDentro(Respuesta,[si,no]).

respuestaSobrePelea(Respuesta):-
	write("No es valida esa respuesta"),nl,
	respuestaSobrePelea(Respuesta).

sacarElementoDeLista(Indice,Lista,Elemento):-sacarElementoDeLista(Indice,0,Lista,Elemento).

sacarElementoDeLista(Indice,Contador,Lista,Elemento):-
	Lista = [Cabeza|Cola],
	Indice=Contador,
	Elemento=Cabeza.
	
sacarElementoDeLista(Indice,Contador,Lista,Elemento):-
	Lista = [Cabeza|Cola],
	Indice\=Contador,
	NuevoContador is Contador+1,
	sacarElementoDeLista(Indice,NuevoContador,Cola,Elemento).

impresionListaNumerada([],_).

impresionListaNumerada([Cabeza|Cola],Contador):-
    write(Contador), write(" = "), write(Cabeza),nl,
    NuevoContador is Contador+1, 
	impresionListaNumerada(Cola, NuevoContador).
	%Indice=Contador.

impresionListaNumerada(Lista):-impresionListaNumerada(Lista,0).

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
	estaDentro(CiudadParaIr,X),
	write("Has escogido :"),
	write(CiudadParaIr),nl.

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
	length(PokemonesJugador,LongitudPokemones),
	Suma is LongitudHuevos + LongitudPokemones,
	Suma <7.

preguntarGuardadoHuevos(Respuesta):-
	write("Te guardo el huevo en la Mochila? o se enviara a Bill (si/no)"),nl,
	read(Respuesta),
	estaDentro(Respuesta,[si,no]).
preguntarGuardadoHuevos(Respuesta):-
	write("No es valida esa respuesta"),nl,
	preguntarGuardadoHuevos(Respuesta).

preguntarQuePokemonVasaSacar(Respuesta):-
     write("¿Que pokemon vas a mandar a Bill? Pon un indice"),nl,
     pokemonesJugador(Pokemones),
     length(Pokemones,LongitudPokemones),
     impresionListaNumerada(Pokemones),nl,
	 read(Respuesta).
	
mandarPokemonABill(Indice):-
  	pokemonesJugador(ListaPokemonesJugador),
  	pokemonesconBill(ListaPokemonesconBill),
	sacarElementoDeLista(Indice,ListaPokemonesJugador,PokemonEnviadoABill),
	write("Has enviado a Bill, el pokemon: "),
	write(PokemonEnviadoABill),nl,
	eliminarElemento(PokemonEnviadoABill, ListaPokemonesJugador, NuevaLista),
	%write(NuevaLista),
    retractall(pokemonesJugador(ListaPokemonesJugador)),
    assert(pokemonesJugador(NuevaLista)),
    retractall(pokemonesconBill(ListaPokemonesconBill)),
    assert(pokemonesconBill([PokemonEnviadoABill|ListaPokemonesconBill])).

eliminarElemento(Elemento,Lista,ListaSinElemento):-eliminarElemento(Elemento,Lista,[],ListaSinElemento).
eliminarElemento(_,[],ListaSinElemento,ListaSinElemento).
eliminarElemento(Elemento, [Elemento|Cola] ,ListaSinElemento,Resultado):-
	eliminarElemento(Elemento,Cola,ListaSinElemento,Resultado).
eliminarElemento(Elemento, [ElementoDistinto|Cola] ,ListaSinElemento,Resultado):-
	eliminarElemento(Elemento,Cola,[ElementoDistinto|ListaSinElemento],Resultado).	

/*eliminarElemento(Elemento,Lista,ListaSinElemento):-eliminarElemento(Elemento,Lista,[],ListaSinElemento).
eliminarElemento(_,[],ListaSinElemento,ListaSinElemento):-write("igualando acumulador a la variable resultado.").
eliminarElemento(Elemento, [Elemento|Cola] ,ListaSinElemento,Resultado):-
	write(Elemento), write(" es igual al a la cabeza: "),write(Elemento),write(" No se agrega a la lista"),nl,
	eliminarElemento(Elemento,Cola,ListaSinElemento,Resultado).
eliminarElemento(Elemento, [ElementoDistinto|Cola] ,ListaSinElemento,Resultado):-
	write(Elemento), write(" no es igual al a la cabeza: "),write(ElementoDistinto),write(" si se agrega a la lista"),nl,
	eliminarElemento(Elemento,Cola,[ElementoDistinto|ListaSinElemento],Resultado).	*/


%BATALLAS ¿QUIERES SER UN MAESTRO POKEMON?
actualizarEstadoDePelea(Pokemon1,Pokemon2):-
retractall(pokemonesPeleando(_,_)),
assert(pokemonesPeleando(Pokemon1,Pokemon2)).

ponganseaPelear(MiPokemon, PokemonEnemigo):-
	actualizarEstadoDePelea(MiPokemon,PokemonEnemigo),
	elegirAtaque(MiPokemon, Ataque),
	MiPokemon=[_,_,_,_,_,Nivel|_],
	bajarPokemon(Ataque,PokemonEnemigo,Nivel,PokemonEnemigoAct),
	actualizarEstadoDePelea(MiPokemon,PokemonEnemigoAct),
	ataqueRandomEnemigo(PokemonEnemigo,AtaqueEnemigo),
	PokemonEnemigo=[_,_,_,_,_,NivelEnemigo|_],
	PokemonEnemigoAct=[_,_,Salud|_],
	Salud >0,

	bajarPokemon(AtaqueEnemigo,MiPokemon,NivelEnemigo,MiPokemonActualizado),
	actualizarEstadoDePelea(MiPokemonActualizado,PokemonEnemigoAct),
	MiPokemonActualizado=[_,_,Saludmipokemon|_],
	Saludmipokemon >0.

ataqueRandomEnemigo(PokemonEnemigo, Ataque):-

PokemonEnemigo=[_,_,_,Ataques|_],
length(Ataques, LongitudAtaques),
random(0,LongitudAtaques, RandomIndice),
sacarElementoDeLista(RandomIndice,Ataques,Ataque),
write("El enemigo escogio el Ataque: "), write(Ataque),nl.  

elegirAtaque(TuPokemon,Ataque):-
 write("Tienes estos ataques: "),nl,
 TuPokemon=[_,_,_,Ataques|_],
 impresionListaNumerada(Ataques),nl,
 read(Respuesta),
 sacarElementoDeLista(Respuesta,Ataques,Ataque).

 elegirAtaque(TuPokemon,Ataque):- elegirAtaque(TuPokemon,Ataque).

 bajarPokemon(Ataque,PokemonEnemigo, Nivel,PokemonEnemigoActualizado):-
 	PokemonEnemigo=[Nombre,Tipo,Salud|Cola],
 	habilidad(Ataque,PoderAtaque),
 	DTotal is PoderAtaque+((PoderAtaque*Nivel)/5),
 	SaludNueva is Salud-DTotal,
 	PokemonEnemigoActualizado=[Nombre,Tipo,SaludNueva|Cola],
 	write("El ataque "), write(Ataque),write( " bajo "), write(DTotal), write(" de vida, queda "), write(SaludNueva), write(" restante."),nl,nl.

reemplazarenIndice([_|T],0,E,[E|T]).
reemplazarenIndice([H|T],P,E,[H|R]) :-
    P > 0, NP is P-1, reemplazarenIndice(T,NP,E,R).