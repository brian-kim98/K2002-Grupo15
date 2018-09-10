object rolando{
	const property hechizos = [espectroMalefico, hechizoBasico]
	var property hechizoPreferido = espectroMalefico
	var property basePoder = 3

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


