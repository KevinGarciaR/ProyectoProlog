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
	pokemonesconBill/1,
	dineroJugador/1,
	ciudadActual/1,
	ciudadDestino/1.

%Nombre, costo, probabilidad de atrapar el Pokemon
pokebolas([[normal,30,40],[azul,60,60],[negra,100,90]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%                 Estado del Juego                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inicializarVariables:-
% Borrar información del Estado
retractall(pokebolasJugador(_)),
retractall(huevosJugador(_)),
retractall(pokemonesJugador(_)),
retractall(pokemonesPeleando(_)),
retractall(pokemonesconBill(_)),
retractall(dineroJugador(_)),
retractall(ciudadActual(_)),
retractall(ciudadDestino(_)),


%Lista con las pokebola que cuenta el Jugador
assert(pokebolasJugador([
				 [normal,30,40],
				 [azul,60,60],
				 [negra,100,90]
				])),
%Lista con los Huevos  con los que cueta el jugador
assert(huevosJugador([[electrico,80],												
			   [electrico,80],
			   [electrico,80],
			   [electrico,80]
			 ])), 
%Lista con los Pokemones con los que cuenta el jugador
assert(pokemonesJugador([[pikachu,  electrico,60,[ impactrueno,   cabezazo,	tacleada,	chispa],    muerto, 1, 0],
				  [voltorb,  electrico,10,[ agilidad,      destello,    impactrueno,chispa],	muerto, 1, 0]])),

%Lista con los objetos que se envian a Bill
assert(pokemonesconBill([])),
%Saldo del jugador
assert(dineroJugador(1000)),
assert(ciudadActual(culiacan)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Tipo, distancia para nacer
huevos([[electrico,80],[agua,60],[fuego,90],[normal,30],[tierra,50]]).

% pokemones: Nombre,     tipo, salud,                  ataques,                    estado, nivel, experiencia
pokemones([
		  [pikachu,  electrico, 100,[ impactrueno,   cabezazo,	tacleada,	 chispa],    vivo, 1, 0],
		  [electabuzz,electrico,100,[impactrueno,   amago,      grunido,     chispa],    vivo, 1, 0],
          [voltorb,  electrico, 100,[ agilidad,      destello,  impactrueno, chispa],	 vivo, 1, 0],

		  [squirtle, agua,100,  [burbuja,     cabezazo,     mordisco,         latigo],			vivo, 1, 0],
		  [croconaw, agua,100,  [giro_rapido, placaje,      latigo,           hidropulso],      vivo, 1, 0],
		  [krabby,   agua,100,  [hidropulso,  danza ,	    hidrobomba,       burbuja	 ], 	vivo, 1, 0],

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
habilidad(impactrueno,20).
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


 mostrarOpcionesDeViaje(Origen):-  
	 write("Destinos: "),nl,	 
	 distancia2(Origen,X,_),	 
	 tab(1),write(X),nl,
	 false.

 mostrarOpcionesDeViaje(_):- 
     true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                    implementada de tienda         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 tienda:-
	write("******   Lista de objectos que puedes comprar    ******"),nl,nl,
	write("Num    Objecto      precio"),nl,
	write("0   Pokebola norma     30"),nl,
	write("1   Pokebola azul      60"),nl,
	write("2   Pokebola negra     100"),nl,

	dineroJugador(Dinero),nl,
	write("Tu cuentas con un saldo de : "), 
	write(Dinero),nl,
	tiendaRespuesta(Dinero).

tiendaRespuesta(Dinero):-
	write("Seleccione el numero de la opcion deseada : "),nl,
	read(Respuesta),
	pokebolas(Pokebolas),
	nth0(Respuesta,Pokebolas,PokebolaElegida),
	PokebolaElegida=[_,PrecioPokebola,_],nl,
	tiendaCompraPokebola(Dinero,PrecioPokebola,PokebolaElegida).
	
	
tiendaRespuesta(Dinero):-
	write("ERROR: Opcion incorrecta vuelva a intentarlo"),nl,nl,
	tiendaRespuesta(Dinero).

tiendaCompraPokebola(Dinero,PrecioPokebola,Pokebola):-
	Dinero > PrecioPokebola,	
	agregarPokebolaALaMochila(Pokebola),
	NuevoDinero is Dinero-PrecioPokebola,nl,nl,
	retractall(dineroJugador(_)),
	assert(dineroJugador(NuevoDinero)).
	
tiendaCompraPokebola(Dinero,PrecioPokebola,_):-
	write(Dinero),write(PrecioPokebola),nl,
	write("No cuentas con el saldo suficiente, regresa cuando tengas mas dinero"),nl.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&
%%%%%%%%%%%%%%%%%%%%%%%%%%%              Opciones principales del juego	      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Inicio del juego
juegoPokemon:-
  inicializarVariables,
  caminarASiguienteCiudad.
  %llegasASiguienteCiudad.

caminarASiguienteCiudad:-
  preguntarCiudad(CiudadParaIr),
  random(1,4, Random),
  posibilidadAlCaminar(Random),
  write("Has llegado a la ciudad "),write(CiudadParaIr),write(", que deseas hacer: "),nl,
  opcionesDeCiudad.

%Encontro pokebola en el trayecto
posibilidadAlCaminar(1):-
	write("Has encontrado una pokebola en tu trayecto."),nl,
	pokebolas(Pokebolas), 							%unificacion las lista de pokebolas a la variable
	length(Pokebolas,Longitud),
	random(0,Longitud, Random),
	sacarElementoDeLista(Random,Pokebolas,Pokebola),
	agregarPokebolaALaMochila(Pokebola).

%Encontro un huevo en el trayecto
posibilidadAlCaminar(2):-
	encontrarHuevo(_).

posibilidadAlCaminar(3):-
	sacarPokemonRandom(Pokemon),
	Pokemon= [Cabeza|_],
	write("Has encontrado al pokemon: "), write(Cabeza), write(" deseas pelear con el? (si/no)"), nl,
	read(Respuesta),
	respuestaSobrePelea(Respuesta,Pokemon, Cabeza).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&
%%%%%%%%%%%%%%%%%%%%%%%%%%%              Opciones de Ciudad     		      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



opcionesDeCiudad:-
	write("0 = Ir a la tienda"),nl,
	write("1 = Ir a enfermeria"),nl,
	write("2 = Ir al gimnasio"),nl,
	write("3 = Ir a la siguiente ciudad"), nl,nl,
	read(Respuesta),
	opcionesDeCiudadRespuesta(Respuesta).

opcionesDeCiudad:-
	write("Respuesta invalida vuelve a intentarlo"),nl,
	opcionesDeCiudad.	

opcionesDeCiudadRespuesta(0):-
	tienda,
	opcionesDeCiudad.
			
opcionesDeCiudadRespuesta(1):-
	curarPokemones,
	opcionesDeCiudad.

opcionesDeCiudadRespuesta(2):-


	write("").	
opcionesDeCiudadRespuesta(3):-
	 caminarASiguienteCiudad.	

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&
%%%%%%%%%%%%%%%%%%%%%%%%%%%              Encotrar huevo      				  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encontrarHuevo(Huevo):-
	huevos(Huevos), %unificacion las lista de huevos a la variable
	length(Huevos,Longitud),
	random(0,Longitud, Random),
	sacarElementoDeLista(Random,Huevos,Huevo),
	Huevo=[Cabeza|_],
	write("Has encontrado un Huevo en tu trayecto, de tipo: "), write(Cabeza),nl,
	hayEspacio,
	agregarHuevoALaMochila(Huevo).

encontrarHuevo(Huevo):-
    preguntarGuardadoHuevos(Respuesta), % Preguntar se se conservara huevo o se enviara a Bill
    posibilidadConHuevo(Respuesta,Huevo).

posibilidadConHuevo(si,Huevo):-
    preguntarQuePokemonVasaSacar(Indice),
   	mandarPokemonABill(Indice),
   	agregarHuevoALaMochila(Huevo).
posibilidadConHuevo(no,_):-
	true.%manda huevo a bill

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&
%%%%%%%%%%%%%%%%%%%%%%%%%%%              Posibilidad al caminar 3        	  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

respuestaSobrePelea(no,_,_):-
write("Te perdiste de un gran Pokemon :(").

respuestaSobrePelea(si,PokemonEnemigo, NombreEnemigo):-
write("Preparado?"),nl,
write("Necesitas escoger a un Pokemon para enfrentar a "), write(NombreEnemigo),nl,
    pokemonesJugador(Pokemones),
	impresionListaNumerada(Pokemones),nl,
	read(Indice),
	sacarElementoDeLista(Indice,Pokemones,TuPokemon),
	peleaPokemones(TuPokemon, PokemonEnemigo).

respuestaSobrePelea(si,PokemonEnemigo, NombreEnemigo):-respuestaSobrePelea(si,PokemonEnemigo, NombreEnemigo).


peleaPokemones(Mipokemon,Pokemonenemigo):-
	ponganseaPelear(Mipokemon,Pokemonenemigo),
	pokemonesPeleando(MiPokemon1,PokemonEnemigo1),
	ponganseaPelear(MiPokemon1,PokemonEnemigo1),
	pokemonesPeleando(MiPokemon2,PokemonEnemigo2),
	ponganseaPelear(MiPokemon2,PokemonEnemigo2),
	pokemonesPeleando(MiPokemon3,PokemonEnemigo3),
	ponganseaPelear(MiPokemon3,PokemonEnemigo3),
	pokemonesPeleando(MiPokemon4,PokemonEnemigo4),
	% write(" Mi pokemon final:"),write(MiPokemon4),nl,
	% write(" El pokemon enemigo final:"),write(PokemonEnemigo4),nl,
	quienGano(Mipokemon,MiPokemon4,PokemonEnemigo4). 

peleaPokemones(Mipokemon,_):-
	pokemonesPeleando(MiPokemon4,PokemonEnemigo4),
	% write(" Mi pokemon final:"),write(MiPokemon4),nl,
	% write(" El pokemon enemigo final:"),write(PokemonEnemigo4),nl,
	quienGano(Mipokemon,MiPokemon4,PokemonEnemigo4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actualizarPokemon(Mipokemon,MiPokemonFinal):-
	pokemonesJugador(Pokemones),
	actualizarEstadoDePokemon(MiPokemonFinal,MiPokemonActualizado ),
	nth0(IndiceResultado,Pokemones,Mipokemon),
	reemplazarenIndice(Pokemones, IndiceResultado, MiPokemonActualizado, NuevaLista),
	retractall(pokemonesJugador(_)),
	assert(pokemonesJugador(NuevaLista)).

actualizarEstadoDePokemon(Pokemon, PokemonNuevo):-
	Pokemon=[Nombre,Tipo,Salud,Ataques,_,Nivel,Experiencia],
	Salud=<0,
	PokemonNuevo=[Nombre,Tipo,0,Ataques,muerto,Nivel,Experiencia].


actualizarEstadoDePokemon(Pokemon, PokemonNuevo):-
	Pokemon=[Nombre,Tipo,Salud,Ataques,_,Nivel,Experiencia],
	Salud<60,
	PokemonNuevo=[Nombre,Tipo,Salud,Ataques,critico,Nivel,Experiencia].

actualizarEstadoDePokemon(Pokemon, PokemonNuevo):-
	Pokemon=[Nombre,Tipo,Salud,Ataques,_,Nivel,Experiencia],
	PokemonNuevo=[Nombre,Tipo,Salud,Ataques,vivo,Nivel,Experiencia].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
actualizarExperienciaPokemon(Pokemon, PokemonNuevo):-	%si sumatoria de experiencia supera 100 se aumenta el nivel y exériencia=0
	Pokemon=[Nombre,Tipo,Salud,Ataques,Estado,Nivel,Experiencia],
	NuevaExperiencia is Experiencia + 35,
	NuevaExperiencia>=100,
	NuevoNivel is Nivel+1,
	checarEvolucion(Pokemon, NuevoNivel, PokemonNuevo),
	PokemonNuevo=[Nombre,Tipo,Salud,Ataques,Estado,NuevoNivel,0].

actualizarExperienciaPokemon(Pokemon, PokemonNuevo):- %Si la sumatoria de la experiencia adquirida no es mayor a 100 entra a aqui
	Pokemon=[Nombre,Tipo,Salud,Ataques,Estado,Nivel,Experiencia],
	NuevaExperiencia is Experiencia + 35,
	PokemonNuevo=[Nombre,Tipo,Salud,Ataques,Estado,Nivel,NuevaExperiencia].


checarEvolucion(Pokemon,NivelNuevo,PokemonNuevo):-
	Pokemon=[Nombre,Tipo,Salud,Ataques,Estado,Nivel,Experiencia],
	evolucion(Nombre, NombrePokemonEvolucionado,NivelNuevo),
	PokemonNuevo= [NombrePokemonEvolucionado,Tipo,100,Ataques,vivo,Nivel,0],
	write("Tu Pokemon "), write(Nombre), write(" ha evolucionado a "), write(NombrePokemonEvolucionado).


	checarEvolucion(Pokemon,NivelNuevo,Pokemon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quienGano(Mipokemon,MiPokemonFinal,PokemonEnemigoFinal):-    			%Si gano nuestro pokemon
	 MiPokemonFinal=[NombreMiPokemon,_,SaludFinal|_],
	 PokemonEnemigoFinal=[_,_,SaludEnemigoFinal|_],
	 SaludFinal > SaludEnemigoFinal,
	 actualizarExperienciaPokemon(MiPokemonFinal, MiPokemonFinalActualizado),
	 actualizarPokemon(Mipokemon,MiPokemonFinalActualizado),
 	write("Gano mi Pokemon "), write(NombreMiPokemon),nl, 
 	capturarPokemonDePelea(PokemonEnemigoFinal).

 	 quienGano(Mipokemon,MiPokemonFinal,PokemonEnemigoFinal):-  						%Si gano el pokemon enemigo
	 MiPokemonFinal=[_,_,SaludFinal|_],
	 PokemonEnemigoFinal=[NombrePokemonEnemigo,_,SaludEnemigoFinal|_],
	 SaludFinal < SaludEnemigoFinal,
	  actualizarPokemon(Mipokemon,MiPokemonFinal),
	 write("Gano el pokemon enemigo, sigue entrenando a tu Pokemon "), write(NombrePokemonEnemigo),nl.
 	
 capturarPokemonDePelea(PokemonACapturar):-
    pokebolasJugador(HayPokebolas),
 	length(HayPokebolas, LongitudPokebolas),
 	LongitudPokebolas > 0,
 	PokemonACapturar=[NombrePokEne,_,_|_],
 	write("Has vencido al pokemon: "), write(NombrePokEne), write(" deseas capturarlo: (si/no)"), nl,
	read(Respuesta),
	respuestaCapturarPokemon(Respuesta,PokemonACapturar).

capturarPokemonDePelea(_):-
    write("No tienes Pokebolas. Lo siento").

respuestaCapturarPokemon(no,PokemonACapturar):-
	PokemonACapturar=[NombrePokEne,_,_|_],
	write("Has dejado a "), write(NombrePokEne),nl. 


respuestaCapturarPokemon(si,PokemonACapturar):-
	write("Lista de pokebolas"),nl,nl,
	pokebolasJugador(PokebolasJ),
	impresionListaNumerada(PokebolasJ),nl,
	read(IndesP),
	nth0(IndesP, PokebolasJ, PokebolaElegida), 
	tirarPokebola(PokebolaElegida,PokemonACapturar).

	
respuestaCapturarPokemon(si,PokemonACapturar):-
	write("Valor invalido vuelve a intentarlo"),nl,
	respuestaCapturarPokemon(si,PokemonACapturar).

tirarPokebola(PokebolaElegida,PokemonACapturar):-
	random(1,101,NumeroAzar),
	PokebolaElegida=[_,_,ProPokebola],
	NumeroAzar < ProPokebola,   %el numero al azar debe ser menor a la probabilidad de  la pokebola
	pokebolasJugador(PokebolasActuales),
	agregarPokemonALaMochila(PokemonACapturar,PokebolaElegida),
	eliminarElemento(PokebolaElegida,PokebolasActuales,ListaSinElemento),
	retractall(pokebolasJugador(PokebolasActuales)),
	assert(pokebolasJugador(ListaSinElemento)).


tirarPokebola(PokebolaElegida,_):-		%Se eliminara la poquebola aque no se capture el pokemon
	pokebolasJugador(PokebolasActuales),
	eliminarElemento(PokebolaElegida,PokebolasActuales,ListaSinElemento),
	retractall(pokebolasJugador(PokebolasActuales)),
	assert(pokebolasJugador(ListaSinElemento)),
	write("No se ha capturado el pokemon"), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


agregarPokemonALaMochila(NuevoPokemon,_):-
	hayEspacio,
	pokemonesJugador(Pokemones),
	append(Pokemones,[NuevoPokemon],NuevaLista),
	retractall(pokemonesJugador(Pokemones)),
	assert(pokemonesJugador(NuevaLista)),
	write("Se agrego exitosamente el pokemon a la mochila"),nl.

agregarPokemonALaMochila(NuevoPokemon,PokebolaElegida):-
	preguntarGuardadoPokemon(Respuesta), % Preguntar 
    posibilidadConPokemon(Respuesta,NuevoPokemon,PokebolaElegida).

preguntarGuardadoPokemon(Respuesta):-
	write("¿Quieres sacar un pokemon para guardar el nuevo? (si/no)"),nl,
	read(Respuesta),
	estaDentro(Respuesta,[si,no]).
preguntarGuardadoPokemon(Respuesta):-
	write("No es valida esa respuesta"),nl,
	preguntarGuardadoPokemon(Respuesta).

posibilidadConPokemon(si,Pokemon,Pokebola):-
    preguntarQuePokemonVasaSacar(Indice),
   	mandarPokemonABill(Indice),
	agregarPokemonALaMochila(Pokemon,Pokebola).

posibilidadConPokemon(no,Pokemon,_):-
	pokemonesconBill(PokemonesBill),
	retractall(pokemonesconBill(_)),
	assert(pokemonesconBill([Pokemon|PokemonesBill])),
   	write("Mandaste el pokemon a bill"),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&
%%%%%%%%%%%%%%%%%%%%%%%%%%%                       hay espacio                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hayEspacio:-
	pokemonesJugador(PokemonesJugador),
	huevosJugador(HuevosJugador),
	length(HuevosJugador,LongitudHuevos),
	length(PokemonesJugador,LongitudPokemones),
	Suma is LongitudHuevos + LongitudPokemones,
	Suma <6.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&
%%%%%%%%%%%%%%%%%%%%%%%%%%%           Sacar Elemento de la de una lista       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sacarElementoDeLista(Indice,Lista,Elemento):-sacarElementoDeLista(Indice,0,Lista,Elemento).

sacarElementoDeLista(Indice,Contador,Lista,Elemento):-
	Lista = [Cabeza|_],
	Indice=Contador,
	Elemento=Cabeza.

sacarElementoDeLista(Indice,Contador,Lista,Elemento):-
	Lista = [_|Cola],
	Indice\=Contador,
	NuevoContador is Contador+1,
	sacarElementoDeLista(Indice,NuevoContador,Cola,Elemento).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&
%%%%%%%%%%%%%%%%%%%%%%%%%%%            	     Lista numerada                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
impresionListaNumerada([],_).

impresionListaNumerada([Cabeza|Cola],Contador):-
    write(Contador), write(" = "), write(Cabeza),nl,
    NuevoContador is Contador+1,
	impresionListaNumerada(Cola, NuevoContador).

impresionListaNumerada(Lista):-impresionListaNumerada(Lista,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&
%%%%%%%%%%%%%%%%%%%%%%%%%%%           Agregar una pokebola a la mochila 	  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

agregarPokebolaALaMochila(NuevaPokebola):-
   pokebolasJugador(Pokebolas),
   retractall(pokebolasJugador(Pokebolas)),
   assert(pokebolasJugador([NuevaPokebola|Pokebolas])),
   write("Se agrego exitosamente la pokebola a la mochila"),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&

agregarHuevoALaMochila(NuevoHuevo):-
	huevosJugador(Huevos),
	retractall(huevosJugador(Huevos)),
	assert(huevosJugador([NuevoHuevo|Huevos])),
	write("Se agrego exitosamente el huevo a la mochila"),nl.

preguntarCiudad(CiudadParaIr):-
	ciudadActual(CiudadActual),
	mostrarOpcionesDeViaje(CiudadActual), 
	ciudades(X),
	read(CiudadParaIr),
	estaDentro(CiudadParaIr,X),nl,
	write("Has escogido :"),
	write(CiudadParaIr),nl,
	retractall(ciudadDestino(_)),
	assert(ciudadDestino(CiudadParaIr)).


preguntarCiudad(CiudadParaIr):-
	write("No es valida esa ciudad"),nl,
	preguntarCiudad(CiudadParaIr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&
%%%%%%%%%%%%%%%%%%%%%%%%%%%                       Estas dentro           	  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Saber si un elemento esta dentro de una lista
estaDentro(Elemento,[Elemento|_]).

estaDentro(Elemento,[_|Cola]):-
	estaDentro(Elemento,Cola).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&



preguntarGuardadoHuevos(Respuesta):-
	write("Te guardo el huevo en la Mochila? (si/no)"),nl,
	read(Respuesta),
	estaDentro(Respuesta,[si,no]).
preguntarGuardadoHuevos(Respuesta):-
	write("No es valida esa respuesta"),nl,
	preguntarGuardadoHuevos(Respuesta).

preguntarQuePokemonVasaSacar(Respuesta):-
     write("¿Que pokemon vas a mandar a Bill? Pon un indice"),nl,
     pokemonesJugador(Pokemones),
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
	PokemonEnemigo=[_,_,_,_,_,NivelEnemigo|_],
	PokemonEnemigoAct=[_,_,Salud|_],
	Salud >0,
	ataqueRandomEnemigo(PokemonEnemigo,AtaqueEnemigo),
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

% Si se descomenta esta linea va a causar problemas con el metodo ponganseaPelear
 % elegirAtaque(TuPokemon,Ataque):- elegirAtaque(TuPokemon,Ataque).

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


%%Ir al Hospital a Curar los Pokemones.
curarPokemones:-
	pokemonesJugador(PokemonesActuales),
	recorreyCuraPokemones(PokemonesActuales,PokemonesCurados),
	retractall(pokemonesJugador(PokemonesActuales)),
	assert(pokemonesJugador(PokemonesCurados)),
	write("Se han curado tus pokemones satisfactoriamente.").

recorreyCuraPokemones(PokemonesActuales,[NuevoPokemon|PokemonesCurados]):-
 PokemonesActuales=[[Nombre,Tipo,_,Ataques,_|Cola]|PokemonesRestantes],
 NuevoPokemon=[Nombre,Tipo,100,Ataques,vivo|Cola],
 recorreyCuraPokemones(PokemonesRestantes,PokemonesCurados).


 recorreyCuraPokemones([],[]).


%% [1|PROMESA1],
%% PROME1 = [2|PROMESA2]= [2|[3]]
%% PREMESA2 = [3|PROMESA3] = [3].
%% PROMESA3 =[]
	
%% [3|[]] = [3]
%% [2|[3]] = [2,3].
%% [1|[2,3]]= [1,2,3]

