%PuntoA
mira(juan, himym).
mira(juan, futurama).
mira(juan, got).

%agregamos para entrega 2
mira(pedro, got).
%-----------------------

mira(nico, starWars).
mira(nico, got).

mira(maiu, starWars).
mira(maiu, onePiece).
mira(maiu, got).

mira(gaston, hoc).
%no ponemos a Alf por el Principio del Universo Cerrado, ya que no mira ninguna serie, pero lo podemos definir como persona

%tampoco agregamos la Serie Mad Men ya que nadie mira esa serie, por lo tanto al utilizar el principio del universo cerrado, cuando se consulte por ella dara false

planeaVer(juan, hoc).
planeaVer(aye, got).
planeaVer(gaston, himym).

%temporada (serie, temporada, episodios)
temporada(got, 3, 12).
temporada(got, 2, 10).
temporada(himym, 1, 23).
temporada(drHouse, 8, 16).
%no agregamos las temporadas de la serie Mad Men ya que por princpio de universo cerrado, cuando se consulte por ella, respondera false,
% ya que no sabemos nada acerca de esta serie

%Agrego para el punto2 de la entrega 2
paso(got, 3, 2, plotTwist([suenio, sinPiernas])).
paso(got, 3, 12, plotTwist([fuego, boda])).
paso(superCampeones, 9, 9, plotTwist([suenio, coma, sinPiernas])).
paso(drHouse, 8, 7, plotTwist([coma, pastillas])).
%-----

%Anexo
paso(futurama, 2, 3, muerte(seymourDiera)).
paso(starWars, 10, 9, muerte(emperor)).
paso(starWars, 1, 2, relacion(parentesco, anakin, rey)).
paso(starWars, 3, 2, relacion(parentesco, vader, luke)).
paso(himym, 1, 1, relacion(amorosa, ted, robin)).
paso(himym, 4, 3, relacion(amorosa, swarley, robin)).
paso(got, 4, 5, relacion(amistad, tyrion, dragon)).

%Agregamos para entrega 2
leDijo(pedro, aye, got, relacion(amistad, tyrion, dragon)).
leDijo(pedro, nico, got, relacion(parentesco, tyrion, dragon)).
leDijo(nico, juan, futurama, muerte(seymourDiera)).
%------------------------

leDijo(gaston, maiu, got, relacion(amistad, tyrion, dragon)).
leDijo(nico, maiu, starWars, relacion(parentesco, vader, luke)).
leDijo(nico, juan, got, muerte(tyrion)).
leDijo(aye, juan, got, relacion(amistad, tyrion, john)).
leDijo(aye, maiu, got, relacion(amistad, tyrion, john)).
leDijo(aye, gaston, got, relacion(amistad, tyrion, dragon)).

amigo(nico, maiu).
amigo(maiu, gaston).
amigo(maiu, juan).
amigo(juan, aye).

%PuntoB
esSpoiler(Serie, QuePaso):-
  paso(Serie, _, _, QuePaso).

  %podemos realizar ambos tipos de consultas, las individuales siempre son posibles ya que SWI-Prolog puede buscar mediante su motor de busqueda en la base de datos que le damos y asi dar True o False,
  % y las existenciales son posibles gracias a que el predicado que realizamos es inversible

miraOPlaneaVer(Persona, Serie):-
  planeaVer(Persona, Serie).

miraOPlaneaVer(Persona, Serie):-
  mira(Persona, Serie).

%PuntoC
leSpoileo(Persona1, Persona2, Serie):-
  miraOPlaneaVer(Persona2, Serie),
  leDijo(Persona1, Persona2, Serie, QuePaso),
  esSpoiler(Serie, QuePaso).
  %podemos realizar ambos tipos de consultas, las individuales siempre son posibles ya que SWI-Prolog puede buscar mediante su motor de busqueda en la base de datos que le damos y asi dar True o False,
  % y las existenciales son posibles gracias a que el predicado que realizamos es inversible

%PuntoD
televidenteResponsable(Persona):-
  miraOPlaneaVer(Persona, _),
  not(leSpoileo(Persona, _, _)).

%PuntoE
esFuerte(Algo):-
  paso(_,_,_,Algo),
  Algo \= relacion(amistad,_,_),
  Algo \= plotTwist(_).

%Agrego para el Punto2 de la entrega 2
esFuerte(plotTwist(Giro)):-
  pasoAFinalDeTemporada(Giro),
  not(esCliche(Giro)).

%predicado auxiliar
esFuerteOPopular(Serie):-
  esPopular(Serie).

%para una serie especifica, se cumple que para toda temporada de esa serie, paso algo fuerte en cada una de ellas.
esFuerteOPopular(Serie):-
  temporada(Serie,_,_),
  forall(temporada(Serie,Temporada,_),(paso(Serie,Temporada,_,Algo),esFuerte(Algo))).

%aux para el PUNTO2 de la entrega 2
pasoAFinalDeTemporada(Giro):-
  paso(Serie,Temporada,Capitulo,plotTwist(Giro)),
  temporada(Serie,Temporada,Capitulo).

vieneZafando(Persona, Serie):-
  esFuerteOPopular(Serie),
  miraOPlaneaVer(Persona, Serie),
  not(leSpoileo(_,Persona,Serie)).

%tests
:- begin_tests(esSpoiler).
test(es_Spoiler_muerte_del_emperor_en_starWars,nondet):- esSpoiler(starWars, muerte(emperor)).
test(es_Spoiler_relacion_de_parentesco_entre_anakin_y_el_rey_en_starWars,nondet):- esSpoiler(starWars, relacion(parentesco, anakin, rey)).
test(no_es_Spoiler_muerte_de_pedro_en_starWars, fail):- esSpoiler(starWars, muerte(pedro)).
test(no_es_Spoiler_relacion_de_padre_entre_anakin_y_lavezzi_en_starWars, fail):- esSpoiler(starWars, relacion(parentesco, anakin, lavezzi)).
:- end_tests(esSpoiler).

:- begin_tests(leSpoileo).
test(gaston_le_spoileo_a_maiu_sobre_got):- leSpoileo(gaston, maiu, got).
test(nico_le_spoileo_a_maiu_sobre_starWars,nondet):- leSpoileo(nico, maiu, starWars).
:- end_tests(leSpoileo).

:- begin_tests(televidenteResponsable).
test(juan_aye_y_maiu_son_televidentes_responsables, set(X == [juan, aye, maiu])):- televidenteResponsable(X).
test(nico_no_es_televidente_responsable, fail):- televidenteResponsable(nico).
test(gaston_no_es_televidente_responsable, fail):- televidenteResponsable(gaston).
:- end_tests(televidenteResponsable).

:-begin_tests(vieneZafando).
test(maiu_no_zafa_con_ninguna_serie, fail):- vieneZafando(maiu, _).
test(juan_viene_zafando_con_himym_got_y_hoc, set(X = [himym, got, hoc])):- vieneZafando(juan, X).
test(solo_nico_viene_safando_con_starWars, set(X = [nico])):- vieneZafando(X, starWars).
:- end_tests(vieneZafando).


%2da entrega
%punto1
malaPersona(Persona):-
  forall(leDijo(Persona,Persona2,_,_),leSpoileo(Persona,Persona2,_)).

malaPersona(Persona):-
  leSpoileo(Persona,_,Serie),
  not(mira(Persona,Serie)).

%Punto2
sucesoFuerte(Serie,Suceso):-
  paso(Serie,_,_,Suceso),
  esFuerte(Suceso).

esCliche(Giro):-
  paso(Serie1,_,_,plotTwist(Giro)),
  paso(Serie2,_,_,plotTwist(Giro)),
  Serie2 \= Serie1.

%Punto3
esPopular(got).
esPopular(hoc).
esPopular(starWars).

esPopular(Serie):-
  puntajePopularidad(Serie, Puntaje1),
  puntajePopularidad(starWars, Puntaje2),
  Puntaje1 >= Puntaje2.

puntajePopularidad(Serie, Puntaje):-
  cantidadDeEspectadores(Serie, Cantidad1),
  cantidadDeHabladores(Serie, Cantidad2),
  Puntaje is Cantidad1 * Cantidad2.

cantidadDeEspectadores(Serie, Cantidad):-
  mira(_, Serie),
  findall(Espectador, mira(Espectador, Serie), Espectadores),
  length(Espectadores, Cantidad).

cantidadDeHabladores(Serie, Cantidad):-
  mira(_, Serie),
  findall(Hablador, leDijo(Hablador,_,Serie,_), Habladores),
  length(Habladores, Cantidad).

  %punto 4
fullSpoil(Spoileador, Spoileado):-
  leSpoileo(Spoileador, Spoileado, _).

fullSpoil(Spoileador, Spoileado):-
  amigo(AmigoDelSpoileado, Spoileado),
  Spoileado \= Spoileador,
  fullSpoil(Spoileador, AmigoDelSpoileado).

  %punto 5 - Tests
  :-begin_tests(malaPersona).
  test(gaston_y_nico_son_malas_personas, set(Y=[nico, gaston])):- malaPersona(Y).
  test(pedro_no_es_mala_persona,fail):- malaPersona(pedro).
  :- end_tests(malaPersona).

  :-begin_tests(esFuerte).
  test(la_muerte_de_seymourdiera_es_algo_fuerte_en_futurama , nondet):- esFuerte(muerte(seymourDiera)).
  test(el_plottwist_con_fuego_y_boda_es_algo_fuerte_en_got , nondet):- esFuerte(plotTwist([fuego, boda])).
  test(el_plotTwist_con_la_palabra_suenio_no_es_fuerte , fail):- esFuerte(plotTwist([suenio, sinPiernas])).
  :- end_tests(esFuerte).

  :-begin_tests(esPopular).
  test(futurama_OnePiece_y_Himym_no_son_populares , set(X=[futurama, onePiece, himym]), fail):- esPopular(X).
  :-end_tests(esPopular).

  :-begin_tests(fullSpoil).
  test(nico_hizo_fullSpoil_a_juan_aye_y_maiu , set(X = [aye, juan, maiu, gaston])):- fullSpoil(nico,X).
  test(gaston_hizo_fullSpoil_a_maiu_juan_y_aye, set(Y = [maiu, juan, aye])):- fullSpoil(gaston,Y).
  test(maiu_no_fullSpoil_a_nadie , set(Z = [])):- fullSpoil(maiu,Z).
  :-end_tests(fullSpoil).
