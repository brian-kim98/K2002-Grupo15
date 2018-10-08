class Personaje {

	var property hechizoPreferido = espectroMalefico
	var property basePelea = 1
	const property artefactos = []

	method basePoder() = 3

	method nivelDeHechiceria() = self.basePoder() * self.hechizoPreferido().unidadesDeLucha(self) + mundo.fuerzaOscura()

	method artefactos(nuevosArtefactos) {
		self.artefactos().addAll(nuevosArtefactos)
	}

	method teCreesPoderoso() = self.hechizoPreferido().sosPoderoso()

	method agregaLosArtefactos(unosArtefactos) = self.artefactos().addAll(unosArtefactos)

	method agregaArtefacto(unArtefacto) = self.agregaLosArtefactos([ unArtefacto ])

	method removeArtefacto(unArtefacto) = self.artefactos().remove(unArtefacto)

	method removeTodosLosArtefactos() = self.artefactos().clear()

	method valorDeLucha() = self.basePelea() + self.poderDeArtefactos()

	method poderDeArtefactos() = self.artefactos().sum({ artefacto => artefacto.unidadesDeLucha(self) })

	method mejorLuchadorQueMago() = self.valorDeLucha() > self.nivelDeHechiceria()

	method estasCargado() = self.artefactos().size() >= 5

	method mejorArtefacto() = self.artefactosSin(espejo).max({artefacto => artefacto.unidadesDeLucha(self)})

	method artefactosSin(unArtefacto) = self.artefactos().filter({artefacto => artefacto != unArtefacto})

	method cantidadDeArtefactosSin(unArtefacto) = self.artefactosSin(unArtefacto).size()

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

class Logos{
	var property nombre = null
	var property multiplicador = null
	method poder() = self.nombre().size() * self.multiplicador()
	method sosPoderoso() = self.poder() > 15
	method unidadesDeLucha(portador) = self.poder()
	
}

object mundo {

	var property fuerzaOscura = 5

	method eclipse() {
		fuerzaOscura *= 2
	}

}

class Arma{
	const property unidad = 3
	method unidadesDeLucha(portador) = self.unidad()

}

class CollarDivino {

	var property perlas = 5

	method unidadesDeLucha(portador) = self.perlas()

}

class MascaraOscura{
	var property indiceDeOscuridad = 0
	var property minimo = 4
	method indiceDeOscuridad(unIndice){
		indiceDeOscuridad = unIndice.max(0).min(1)
	}
	method unidadesDeLucha(portador) = (self.minimo()).max(mundo.fuerzaOscura()/2 * self.indiceDeOscuridad())
}

class Armadura{
	var property refuerzo = ningunRefuerzo
	var property valorBase = 2
	
	method unidadesDeLucha(portador){
			return self.valorBase() + self.unidadesDeLucha(portador)
	}
}

object ningunRefuerzo{
	method unidadesDeLucha(portador) = 0

}

class CotaDeMallas{
	var property cantidadUnidadDeLucha = 1
	method unidadesDeLucha(portador) = self.cantidadUnidadDeLucha()
}


object bendicion {

	method unidadesDeLucha(portador) = portador.nivelDeHechiceria()

}

object espejo {

	method cantidadDeArtefactos(portador) = portador.artefactos().size()

	method unidadesDeLucha(portador){
		if(portador.cantidadDeArtefactosSin(self) >= 1){
			return portador.mejorArtefacto().unidadesDeLucha(portador)
		}
		else{
			return 0
		}
	}
}

class LibroDeHechizos{
	const property hechizos = []

	method poder() = self.hechizosPoderosos().sum({hechizo => hechizo.poder()})

	method hechizos(nuevosHechizos){
		self.hechizos().clear()
		self.hechizos().addAll(nuevosHechizos)
	}
	method hechizosPoderosos() = self.hechizos().filter({hechizo => hechizo.sosPoderoso()})
	method unidadesDeLucha(portador) = self.hechizosPoderosos().sum({hechizo => hechizo.unidadesDeLucha(portador)})
	method hechizosSin(unHechizo) = self.hechizos().filter({hechizo => hechizo != unHechizo})
}
