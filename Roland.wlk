object rolando{
	const property hechizos = [espectroMalefico, hechizoBasico]
	const property artefactos = [ espadaDelDestino, collarDivino, mascaraOscura ]
	var property hechizoPreferido = espectroMalefico
	var property basePoder = 3
	var property basePelea = 1

	method nivelDeHechiceria() = self.basePoder() * self.hechizoPreferido().poder() + fuerzaOscura.valor()

	method hechizos(nuevosHechizos){
		self.hechizos().clear()
		self.hechizos().addAll(nuevosHechizos)}

	method hechizoPreferido(nuevoPreferido){
		if(self.hechizos().contains(nuevoPreferido)){
			hechizoPreferido = nuevoPreferido
		}
	}

	method seCreePoderoso() = self.hechizoPreferido().esPoderoso()

	method agregarArtefacto(unArtefacto) = self.artefactos().add(unArtefacto)

	method removerArtefacto(unArtefacto) = self.artefactos().remove(unArtefacto)

	method removerArtefactos() = self.artefactos().clear()

	method valorDeLucha() = self.basePelea() + self.artefactos().sum({ artefacto => artefacto.unidadesDeLucha(self) })

	method mejorLuchadorQueMago() = self.valorDeLucha() > self.nivelDeHechiceria()
}

object espectroMalefico{
	var property nombre = "Espectro Maléfico"
	method poder() = self.nombre().size()
	method esPoderoso() = self.poder() > 15
	method unidadesDeLucha(portador) = self.poder()
}

object hechizoBasico{
	method poder() = 10
	method esPoderoso() = false
	method unidadesDeLucha(portador) = self.poder()
}

object fuerzaOscura{
	var valor = 5
	method valor() = valor
	method eclipse(){valor *= 2}
}

object espadaDelDestino {

	method unidadesDeLucha(portador) = 3

}

object collarDivino {

	var property perlas = 1

	method unidadesDeLucha(portador) = perlas

}

object mascaraOscura {

	method unidadesDeLucha(portador) = 4.max(fuerzaOscura.valor() / 2)

}

object armadura{
	const property refuerzos = [cotaDeMalla, hechizoBasico, espectroMalefico, bendicion]
	var property refuerzoElegido = null

	method refuerzoElegido(nuevoRefuerzo){
		if(self.refuerzos().contains(nuevoRefuerzo)){
			refuerzoElegido = nuevoRefuerzo
		}
	}

	method unidadesDeLucha(portador){
		if(self.refuerzoElegido() != null){
			return 2 + refuerzoElegido.unidadesDeLucha(portador)
		}
		else{
			return 2
		}
	}
}
object cotaDeMalla{method unidadesDeLucha(portador) = 1}

object bendicion{
	method unidadesDeLucha(portador) = portador.nivelDeHechiceria()
}

object espejoFantastico{
	method cantidadDeArtefactos(portador) = portador.artefactos().size()

	method unidadesDeLucha(portador){
		if(self.cantidadDeArtefactos(portador) > 1){
			return portador.artefactos().filter({artefacto => artefacto != self}).max({artefacto => artefacto.unidadesDeLucha(portador)})
		}
		else{
			return 0
		}
	}
}

object libroDeHechizos{
	const property hechizos = []
	method hechizosPoderosos() = self.hechizos().filter({hechizo => hechizo.esPoderoso()})
	method unidadesDeLucha() = self.hechizosPoderosos().sum({hechizo => hechizo.unidadesDeLucha()})
}
