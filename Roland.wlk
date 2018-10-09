class Personaje{
	const property artefactos = []
	var property hechizoPreferido
	var property basePelea = 1
	var property monedas = 100

	method basePoder () = 3

	method nivelDeHechiceria() = self.basePoder() * self.hechizoPreferido().poder() + mundo.fuerzaOscura()

	method agregaLosArtefactos(unosArtefactos) = self.artefactos().addAll(unosArtefactos)
	method agregaArtefacto(unArtefacto) = self.agregaLosArtefactos([unArtefacto])
	method removeArtefacto(unArtefacto) = self.artefactos().remove(unArtefacto)
	method removeTodosLosArtefactos() = self.artefactos().clear()

	method teCreesPoderoso() = self.hechizoPreferido().esPoderoso()

	method valorDeLucha() = self.basePelea() + self.poderDeArtefactos()

	method poderDeArtefactos() = self.artefactos().sum({artefacto => artefacto.unidadesDeLucha(self)})

	method mejorLuchadorQueMago() = self.valorDeLucha() > self.nivelDeHechiceria()

	method estasCargado() = self.artefactos().size() >= 5

	method mejorArtefacto() = self.artefactosSin(espejoFantastico).max({artefacto => artefacto.unidadesDeLucha(self)})

	method artefactosSin(unArtefacto) = self.artefactos().filter({artefacto => artefacto != unArtefacto})

	method cantidadDeArtefactosSin(unArtefacto) = self.artefactosSin(unArtefacto).size()

	method monedas(nuevaCantidad){
		monedas = nuevaCantidad
	}

	method cumplisObjetivo(){
		self.ganasMonedas(10)
	}

	method ganasMonedas(monedasGanadas){
		var acumulador = self.monedas() + monedasGanadas
		self.monedas(acumulador)
	}

}

object hechizoBasico{
	method poder() = 10
	method sosPoderoso() = false
	method unidadesDeLucha(portador) = self.poder()
	method precio() = 10
	method precioRefuerzo(armadura) = armadura.valorBase() + self.precio()
}

class Logos{
	var property nombre
	var property multiplicador
	method poder() = self.nombre().size() * self.multiplicador()
	method sosPoderoso() = self.poder() > 15
	method unidadesDeLucha(portador) = self.poder()
	method precio() = self.poder()
	method precioRefuerzo(armadura) = armadura.valorBase() + self.precio()

}

object mundo{
	var property fuerzaOscura = 5
	method eclipse(){fuerzaOscura *= 2}
}

class Arma{
	const property unidad = 3
	method unidadesDeLucha(portador) = self.unidad()
	method precio() = self.unidad() * 5
}

class CollarDivino{
	var property perlas = 1
	method unidadesDeLucha(portador) = self.perlas()
	method precio() = self.perlas() * 2
}

class MascaraOscura{
	var property indiceOscuridad
	var property minimo = 4
	method unidadesDeLucha(portador) = (self.minimo()).max(mundo.fuerzaOscura()/2 * self.indiceOscuridad())
}

class Armadura{
	var property refuerzo = ningunRefuerzo
	var property valorBase = 2

	method unidadesDeLucha(portador){
			return self.valorBase() + self.refuerzo().unidadesDeLucha(portador)
	}

	method precio() = self.refuerzo().precioRefuerzo()
}

object ningunRefuerzo{
	method unidadesDeLucha(portador) = 0
	method precioRefuerzo(armadura) = 2
}
class CotaDeMalla{
	var property cantidadUnidadDeLucha = 1
	method unidadesDeLucha(portador) = self.cantidadUnidadDeLucha()
	method precioRefuerzo(armadura) = self.cantidadUnidadDeLucha()/2
}

object bendicion{
	method unidadesDeLucha(portador) = portador.nivelDeHechiceria()
	method precioRefuerzo(armadura) = armadura.valorBase()
}

object espejoFantastico{
	method cantidadDeArtefactos(portador) = portador.artefactos().size()

	method unidadesDeLucha(portador){
		if(portador.cantidadDeArtefactosSin(self) >= 1){
			return portador.mejorArtefacto().unidadesDeLucha(portador)
		}
		else{
			return 0
		}
	}

	method precio() = 90
}

class LibroDeHechizos{
	const property hechizos = []
	var property precioPorCadaHechizo = 10

	method poder() = self.hechizosPoderosos().sum({hechizo => hechizo.poder()})

	method hechizos(nuevosHechizos){
		self.hechizos().clear()
		self.hechizos().addAll(nuevosHechizos)
	}

	method hechizosPoderosos() = self.hechizosSin(self).filter({hechizo => hechizo.sosPoderoso()})

	method unidadesDeLucha(portador) = self.hechizosPoderosos().sum({hechizo => hechizo.unidadesDeLucha(portador)})

	method hechizosSin(unHechizo) = self.hechizos().filter({hechizo => hechizo != unHechizo})

	method precio() = self.precioTotalPorHechizos() + self.precioTotalDelHechizo()

	method precioTotalDelHechizo() = self.hechizosPoderosos().sum({hechizo => hechizo.poder()})

	method precioTotalPorHechizos() = self.hechizosSin(self).size() * self.precioPorCadaHechizo()

}
