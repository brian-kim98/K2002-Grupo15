%PuntoA
serie(himym).
serie(futurama).
serie(got).
serie(starWars).
serie(onePiece).
serie(hoc).
serie(madMen).
serie(drHouse).

persona(juan).
persona(nico).
persona(maiu).
persona(gaston).
persona(aye).

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
esPopular(got).
esPopular(hoc).
esPopular(starWars).


planeaVer(juan, hoc).
planeaVer(aye, got).
planeaVer(gaston, himym).

%temporada (serie, temporada, episodios)
temporada(got, 3, 12).
temporada(got, 2, 10).
temporada(himym, 1, 23).
temporada(drHouse, 8, 16).

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

%PuntoB
esSpoiler(Serie, QuePaso):-
  serie(Serie),
  paso(Serie, _, _, QuePaso).

  %podemos realizar ambos tipos de consultas, las individuales siempre son posibles ya que SWI-Prolog puede buscar mediante su motor de busqueda en la base de datos que le damos y asi dar True o False,
  % y las existenciales son posibles gracias a que el predicado que realizamos es inversible

%PuntoC
leSpoileo(Persona1, Persona2, Serie):-
  persona(Persona1),
  persona(Persona2),
  planeaVer(Persona2, Serie),
  leDijo(Persona1, Persona2, Serie, QuePaso),
  paso(Serie, _, _, QuePaso).

  %podemos realizar ambos tipos de consultas, las individuales siempre son posibles ya que SWI-Prolog puede buscar mediante su motor de busqueda en la base de datos que le damos y asi dar True o False,
  % y las existenciales son posibles gracias a que el predicado que realizamos es inversible

leSpoileo(Persona1, Persona2, Serie):-
  persona(Persona1),
  persona(Persona2),
  mira(Persona2, Serie),
  leDijo(Persona1, Persona2, Serie, QuePaso),
  paso(Serie, _, _, QuePaso).

%PuntoD
televidenteResponsable(Persona):-
  persona(Persona),
  not(leSpoileo(Persona, _, _)).

%PuntoE
esFuerte(relacion(parentesco,_,_)).
esFuerte(muerte(_)).
esFuerte(relacion(amorosa,_,_)).

%predicado auxiliar
esFuerteOPopular(Serie):-
  esPopular(Serie).

%para una serie especifica, se cumple que para toda temporada de esa serie, paso algo fuerte en cada una de ellas.
esFuerteOPopular(Serie):-
  temporada(Serie,_,_),
  forall(temporada(Serie,Temporada,_),(paso(Serie,Temporada,_,Algo),esFuerte(Algo))).


vieneZafando(Persona, Serie):-
  esFuerteOPopular(Serie),
  persona(Persona),
  mira(Persona, Serie),
  not(leSpoileo(_,Persona,Serie)).

vieneZafando(Persona, Serie):-
  esFuerteOPopular(Serie),
  persona(Persona),
  planeaVer(Persona, Serie),
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
