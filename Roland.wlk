class Personaje {

	const property artefactos = []
	const property capacidadDeCarga = 200
	var property hechizoPreferido
	var property basePelea = 1
	var property monedas = 100

	method basePoder() = 3

	method nivelDeHechiceria() = self.basePoder() * self.hechizoPreferido().poder() + mundo.fuerzaOscura()

	method agregaLosArtefactos(unosArtefactos) {
		if (self.podesAgregarEstosArtefactos(unosArtefactos).negate()) {
			throw new NoPuedeCargarMas ("Tu personaje no puede cargar este/s Artefacto/S")
		}
		return self.artefactos().addAll(unosArtefactos)
	}

	method losPesosDeTusArtefactos() = self.artefactos().sum({ artefacto => artefacto.cuantoPesas() })

	method podesAgregarEstosArtefactos(artefactosNuevos) = self.losPesosDeTusArtefactos() + artefactosNuevos.sum({ artefacto => artefacto.cuantoPesas() }) <= self.capacidadDeCarga()

	method agregaArtefacto(unArtefacto) = self.agregaLosArtefactos([ unArtefacto ])

	method removeArtefacto(unArtefacto) = self.artefactos().remove(unArtefacto)

	method removeTodosLosArtefactos() = self.artefactos().clear()

	method teCreesPoderoso() = self.hechizoPreferido().esPoderoso()

	method valorDeLucha() = self.basePelea() + self.poderDeArtefactos()

	method poderDeArtefactos() = self.artefactos().sum({ artefacto => artefacto.unidadesDeLucha(self) })

	method mejorLuchadorQueMago() = self.valorDeLucha() > self.nivelDeHechiceria()

	method estasCargado() = self.artefactos().size() >= 5

	method mejorArtefacto() = self.artefactosSin(espejoFantastico).max({ artefacto => artefacto.unidadesDeLucha(self) })

	method artefactosSin(unArtefacto) = self.artefactos().filter({ artefacto => artefacto != unArtefacto })

	method cantidadDeArtefactosSin(unArtefacto) = self.artefactosSin(unArtefacto).size()

	method monedas(nuevaCantidad) {
		monedas = nuevaCantidad
	}

	method cumplisObjetivo() {
		self.ganasMonedas(10)
	}

	method ganasMonedas(monedasGanadas) {
		self.monedas(self.monedas() + monedasGanadas)
	}

	method compraArtefacto(artefactoAComprar) {
		if (self.podesComprarArtefacto(artefactoAComprar).negate()) {
			throw new NoTenesMonedas("Te faltan un par de monedas en la billetera para comprar el artefacto")
		}
		self.agregaArtefacto(artefactoAComprar)
		self.monedas(self.monedas() - artefactoAComprar.precio())
	}

	method podesComprarArtefacto(artefactoAComprar) = artefactoAComprar.precio() <= self.monedas()

	method compraHechizo(nuevoHechizo) {
		if ( self.descuento() + self.monedas() < nuevoHechizo.precio()) {
			throw new NoTenesMonedas()
		}
			self.monedas(self.monedas() -(nuevoHechizo.precio() - self.descuento()).max(0))
			self.hechizoPreferido(nuevoHechizo)
	}
	
	method descuento() = self.hechizoPreferido().precio() / 2
	

}

class NoTenesMonedas inherits Exception {

}

class NoPuedeCargarMas inherits Exception {

}

class NivelFacil {

	method multiplicador() = 1

}

class NivelModerado {

	method multiplicador() = 2

}

class NivelDificil {

	method multiplicador() = 4

}

class NPC inherits Personaje {

	var property nivel = new NivelFacil()

	override method valorDeLucha() = super() * self.nivel().multiplicador()

}

object mundo {

	var property fuerzaOscura = 5

	method eclipse() {
		fuerzaOscura *= 2
	}

}

class Hechizo {

	method poder()

	method unidadesDeLucha(portador) = self.poder()

	method precioRefuerzo(armadura) = armadura.valorBase() + self.precio()

	method precio()
	
	method pesoQueAgrega() = 0
}

class HechizoComercial inherits Logos {

	var property porcentaje = 0.2

	override method poder() = (self.nombre().size() * self.porcentaje()) * self.multiplicador()

}

object hechizoBasico inherits Hechizo {

	override method poder() = 10

	method sosPoderoso() = false

	override method precio() = 10
	


}

class Logos inherits Hechizo {

	var property nombre
	var property multiplicador

	override method poder() = self.nombre().size() * self.multiplicador()

	method sosPoderoso() = self.poder() > 15

	override method precio() = self.poder()
	
}

class LibroDeHechizos {

	const property hechizos = []
	var property precioPorCadaHechizo = 10

	method poder() = self.hechizosPoderosos().sum({ hechizo => hechizo.poder() })

	method hechizos(nuevosHechizos) {
		self.hechizos().clear()
		self.hechizos().addAll(nuevosHechizos)
	}

	method agregarHechizo(unHechizo) {
		self.hechizos().add(unHechizo)
	}

	method hechizosPoderosos() = self.hechizosSin(self).filter({ hechizo => hechizo.sosPoderoso() })

	method unidadesDeLucha(portador) = self.hechizosPoderosos().sum({ hechizo => hechizo.unidadesDeLucha(portador) })

	method hechizosSin(unHechizo) = self.hechizos().filter({ hechizo => hechizo != unHechizo })

	method precio() = self.precioTotalPorHechizos() + self.precioTotalDelHechizo()

	method precioTotalDelHechizo() = self.hechizosPoderosos().sum({ hechizo => hechizo.poder() })

	method precioTotalPorHechizos() = self.hechizosSin(self).size() * self.precioPorCadaHechizo()

}

class Artefacto {

	var property diasDesdeLaCompra = 0
	var property peso

	method cuantoPesas() = self.peso() - self.factorDeCorreccion()

	method factorDeCorreccion() = (self.diasDesdeLaCompra() / 1000).min(1)

}

class Arma inherits Artefacto {

	const property unidad = 3

	method unidadesDeLucha(portador) = self.unidad()

	method precio() = self.unidad() * 5

}

class CollarDivino inherits Artefacto {

	var property perlas = 1

	method unidadesDeLucha(portador) = self.perlas()

	method precio() = self.perlas() * 2

	override method cuantoPesas() = super() + self.perlas() * 5

}

object espejoFantastico inherits Artefacto {

	method cantidadDeArtefactos(portador) = portador.artefactos().size()

	method unidadesDeLucha(portador) {
		if (portador.cantidadDeArtefactosSin(self) >= 1) {
			return portador.mejorArtefacto().unidadesDeLucha(portador)
		} else {
			return 0
		}
	}

	method precio() = 90

}

class MascaraOscura inherits Artefacto {

	var property indiceOscuridad
	var property minimo = 4

	method unidadesDeLucha(portador) = (self.minimo()).max(mundo.fuerzaOscura() / 2 * self.indiceOscuridad())

	override method cuantoPesas() = super() + self.pesoExtra()

	method pesoExtra() = 0.max(self.unidadesDeLucha(self) - 3)

}

class Armadura inherits Artefacto {

	var property refuerzo = ningunRefuerzo
	var property valorBase = 2

	method unidadesDeLucha(portador) {
		return self.valorBase() + self.refuerzo().unidadesDeLucha(portador)
	}

	method precio() = self.refuerzo().precioRefuerzo(self)

	override method cuantoPesas() = super() + self.refuerzo().pesoQueAgrega()

}

object ningunRefuerzo {

	method unidadesDeLucha(portador) = 0

	method precioRefuerzo(armadura) = 2

	method pesoQueAgrega() = 0

}

class CotaDeMalla {

	var property cantidadUnidadDeLucha = 1

	method unidadesDeLucha(portador) = self.cantidadUnidadDeLucha()

	method precioRefuerzo(armadura) = self.cantidadUnidadDeLucha() / 2

	method pesoQueAgrega() = 1

}

object bendicion {

	method unidadesDeLucha(portador) = portador.nivelDeHechiceria()

	method precioRefuerzo(armadura) = armadura.valorBase()

	method pesoQueAgrega() = 0

}
