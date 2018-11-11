import rolando.*
import Refuerzos.*

class Artefacto {

	var property fechaDeCompra = mundo.fechaDeHoy()
	var property porcentajeIVA = 21
	var property peso
	
	method cuantoPesas() = self.peso() - self.factorDeCorreccion()

	method factorDeCorreccion() = (self.diasDesdeLaCompra() / 1000).min(1)
	
	method diasDesdeLaCompra() = mundo.fechaDeHoy() - self.fechaDeCompra()
}

class Arma inherits Artefacto {

	const property unidad = 3

	method unidadesDeLucha(portador) = self.unidad()

	method precio() = self.cuantoPesas() * 5

}

class CollarDivino inherits Artefacto {

	var property perlas = 5

	method unidadesDeLucha(portador) = self.perlas()

	method precio() = self.perlas() * 2

	override method cuantoPesas() = super() + self.perlas() * 0.5

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
	
	method precio() = 10 * self.indiceOscuridad()
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
