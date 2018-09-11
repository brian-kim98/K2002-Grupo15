object rolando {

	var property basePoder = 3
	var property hechizoPreferido = espectroMalefico
	var property basePelea = 1
	const property hechizos = [ espectroMalefico, hechizoBasico ]
	const property artefactos = []

	method nivelDeHechiceria() = self.basePoder() * self.hechizoPreferido().unidadesDeLucha(self) + fuerzaOscura.valor()

	method artefactos(nuevosArtefactos) {
		self.artefactos().addAll(nuevosArtefactos)
	}

	method agregaLosArtefactos(unosArtefactos) = self.artefactos().addAll(unosArtefactos)

	method agregaArtefacto(unArtefacto) = self.agregaLosArtefactos([ unArtefacto ])

	method removeArtefacto(unArtefacto) = self.artefactos().remove(unArtefacto)

	method removeTodosLosArtefactos() = self.artefactos().clear()

	method valorDeLucha() = self.basePelea() + self.artefactos().sum({ artefacto => artefacto.unidadesDeLucha(self) })

	method mejorLuchadorQueMago() = self.valorDeLucha() > self.nivelDeHechiceria()

	method estasCargado() = self.artefactos().size() >= 5

	method mejorArtefacto() = self.artefactos().filter({artefacto => artefacto != espejo}).max({artefacto => artefacto.unidadesDeLucha(self)})

}

object espectroMalefico {

	var property nombre = "Espectro Maléfico"

	method poder() = self.nombre().size()

	method sosPoderoso() = self.poder() > 15

	method unidadesDeLucha(portador) = self.poder()

}

object hechizoBasico {

	method poder() = 10

	method sosPoderoso() = false

	method unidadesDeLucha(portador) = self.poder()

}

object fuerzaOscura {

	var valor = 5

	method valor() = valor

	method eclipse() {
		valor *= 2
	}

}

object espadaDelDestino {

	method unidadesDeLucha(portador) = 3

}

object collarDivino {

	var property perlas = 5

	method unidadesDeLucha(portador) = perlas

}

object mascaraOscura {

	method unidadesDeLucha(portador) = 4.max(fuerzaOscura.valor() / 2)

}

class Armadura {

	const property refuerzosPosibles = [ cotaDeMallas, bendicion, espectroMalefico, hechizoBasico ]
	var property refuerzo = null

	method refuerzo(nuevoRefuerzo) {
		if (self.refuerzosPosibles().contains(nuevoRefuerzo)) {
			refuerzo = nuevoRefuerzo
		}
	}

	method unidadesDeLucha(portador) = if (self.refuerzo() != null) {
		2 + self.refuerzo().unidadesDeLucha(portador)
	} else {
		return 2
	}

}

object cotaDeMallas {

	method unidadesDeLucha(portador) = 1

}

object bendicion {

	method unidadesDeLucha(portador) = portador.nivelDeHechiceria()

}

object espejo {

	method cantidadDeArtefactos(portador) = portador.artefactos().size()

	method unidadesDeLucha(portador){
		if(self.cantidadDeArtefactos(portador) > 1){
			return portador.mejorArtefacto().unidadesDeLucha(portador)
		}
		else{
			return 0
		}
	}
}

object libroDeHechizos{
	const property hechizos = []

	method hechizos(nuevosHechizos){
		self.hechizos().clear()
		self.hechizos().addAll(nuevosHechizos)
	}
	method hechizosPoderosos() = self.hechizos().filter({hechizo => hechizo.sosPoderoso()})
	method unidadesDeLucha(portador) = self.hechizosPoderosos().sum({hechizo => hechizo.unidadesDeLucha(portador)})
}
