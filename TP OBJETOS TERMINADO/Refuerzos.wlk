

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