import rolando.*

class Hechizo {
	
	method sosPoderoso() = self.poder() > 15
	
	method poder()

	method unidadesDeLucha(portador) = self.poder()

	method precioRefuerzo(armadura) = armadura.valorBase() + self.precio()

	method precio() = self.poder()
	
	method pesoQueAgrega() {
		
		if(self.sosHechizoPar()){
			return 2
		}
		else {
			
			return 1
		}
	}
	
	method sosHechizoPar() = self.poder() % 2 == 0
}

class HechizoComercial inherits Logos {

	var property porcentaje = 0.2

	override method poder() = super()*self.porcentaje()

}

object hechizoBasico inherits Hechizo {

	override method poder() = 10

}

class Logos inherits Hechizo {

	var property nombre
	var property multiplicador

	override method poder() = self.nombre().size() * self.multiplicador()

	
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