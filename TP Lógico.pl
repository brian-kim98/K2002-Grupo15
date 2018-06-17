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

paso(futurama, 2, 3, muerte(seymourDiera)).
paso(starWars, 10, 9, muerte(emperor)).
paso(starWars, 1, 2, relacion(parentesco, anakin, rey)).
paso(starWars, 3, 2, relacion(parentesco, vader, luke)).
paso(himym, 1, 1, relacion(amorosa, ted, robin)).
paso(himym, 4, 3, relacion(amorosa, swarley, robin)).
paso(got, 4, 5, relacion(amistad, tyrion, dragon)).

leDijo(gaston, maiu, got, relacion(amistad, tyrion, dragon)).
leDijo(nico, maiu, starWars, relacion(parentesco, vader, luke)).
leDijo(nico, juan, got, muerte(tyrion)).
leDijo(aye, juan, got, relacion(amistad, tyrion, john)).
leDijo(aye, maiu, got, relacion(amistad, tyrion, john)).
leDijo(aye, gaston, got, relacion(amistad, tyrion, dragon)).


esSpoiler(Serie, QuePaso):-
  serie(Serie),
  paso(Serie, _, _, QuePaso).


  leSpoileo(Persona1, Persona2, Serie):-
    persona(Persona1),
    persona(Persona2),
    planeaVer(Persona2, Serie),
    leDijo(Persona1, Persona2, Serie, QuePaso),
    paso(Serie, _, _, QuePaso).

  leSpoileo(Persona1, Persona2, Serie):-
    persona(Persona1),
    persona(Persona2),
    mira(Persona2, Serie),
    leDijo(Persona1, Persona2, Serie, QuePaso),
    paso(Serie, _, _, QuePaso).
